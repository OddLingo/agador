;;;; Obtains weather information from the National Hurricane
;;;; Center.  TWO (Tropical Weather Outlook)
;;; See https://lispcookbook.github.io/cl-cookbook/web-scraping.html

(in-package :AGA)
(defparameter +map+ "https://www.nhc.noaa.gov/xgtwo/two_atl_5d0.png")
(defparameter +info-trop+ "wx-tropical")

;;; Regex patterns for extracing text from the NHC web page.
(defparameter +datestart+ "^\\d+ AM|PM EDT|EST")
(defparameter +textend+ "^</pre>")

;;; General purpose function for fetching preformatted text from
;;; a web page.  The result is a list of text lines.
(defun web-fetch (url)
  (let* ((request (dex:get url))
	 (parsed (lquery:$ (initialize request)))
	 (dom (lquery:$ parsed "div" ".textproduct" "pre"))
	 (texts (lquery:$ dom (serialize)))
	 (text (elt texts 0))
	 )
    (cl-utilities:split-sequence '#\Linefeed text)))

;;; Parse NOAA's unique time format into universal time and speakable
;;; formats.  NOAA web pages have dates in this format:
;;; 715 PM EDT Sat May 16 2020 but we need
;;; Sat Mar 1 19:42:34 2008
(defun noaa-time (line)
  "Convert a NOAA-format time to universal seconds"
  (ppcre:register-groups-bind
   (('parse-integer hrs)
    ampm dow mon
    ('parse-integer day)
    ('parse-integer year))
   ("^(\\d+) (AM|PM) \\w+ (\\w+) (\\w+) (\\d+) (\\d+)" line :sharedp T)
   (let* ((hour (floor (/ hrs 100)))
	  (minute (mod hrs 100))
	  (hr24 (if (equal ampm "PM") (+ 12 hour) hour))
	  ;; Thu Jul 23 19:42:23 2013” 'asctime' format
	  (cnv (format NIL "~a ~a ~d ~2,'0d:~2,'0d:00 EDT ~d"
		       dow mon day hr24 minute year))
	  (uni (+ (* 8 3600) (cl-date-time-parser:parse-date-time cnv))))
     uni
     )))

;;; Fetch the latest tropical storm forecast fro the National Hurricane
;;; Center web site.
(defun get-tropical ()
  "Get an Atlantic Tropical Storm forecast."
  (let* ((lines (web-fetch "https://www.nhc.noaa.gov/gtwo.php"))
	 (univ 0)
	 (temp 0)
	 (text NIL))
    ;; Now remove any lines that are not necessary for speech output.
    (dolist (line lines)
      (cond
	;; Separate out the date line
	((setf temp (noaa-time line)) (setf univ temp))
	;; NNNN marks the end of a message
	((equal line "NNNN") (return))
	;; Ignore the forecaster's name
	((ppcre:scan "^Forecaster" line) (return))
	((equal line "&amp;&amp;") (return))
	;; Anything else goes into the result.
	((> univ 0) (push line text))
	(T T)))
    (cons univ (agu:string-from-list (nreverse text)))))

;;; The most recent tropical forecast is in the 'info' databse.
;;; We can fetch it from there when requested.
(defun wx-repeat ()
  "Repeat the most recent weather update"
  (agm:db-start)
  (let ((old (agm:get-info +info-trop+)))
    (agm:db-commit)
    (if old
      (agu:term
       "At ~a, the National Weather Service reported~%~a~% "
       (speakable-time (car old))
       (cdr old))
      (agu:term "No tropical forecast is available~%"))))

;;;; The background operation to check for new forecasts from
;;;; time to time.
(defun wx-tropical ()
  (let* ((latest (get-tropical))
	 (date (car latest)))
    ;; Ignore reports older than one day.
    (if (> (age date) 86400)
	(agu:term
	 "There have been no tropical weather updates since ~a.~%"
	 (speakable-time date))
	;; We have a new report!  Remember and report it.
	(progn
	  (agm:db-start)
	  (agm:put-info +info-trop+ latest)
	  (agm:db-commit)
	  (wx-repeat)))

  ;; Schedule the next report 4 hours ahead.
  (agu:sked-later 14400 #'wx-tropical)))
