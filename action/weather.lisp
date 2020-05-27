;;;; Obtains weather information from the National Hurricane
;;;; Center.  TWO (Tropical Weather Outlook)
;;; See https://lispcookbook.github.io/cl-cookbook/web-scraping.html

(in-package :AGA)
(defparameter +map+ "https://www.nhc.noaa.gov/xgtwo/two_atl_5d0.png")
(defvar *lastnhc* NIL)
(defvar *last* 0)

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

;;; Save the current date and forecast.
(defun save-wx (key date text)
  (agm:db-start)
  (agm:put-info key (cons date text))
  (agm:db-commit))

(defparameter +weekdays+ (make-hash-table :test 'equal))
(setf (gethash "Sat" +weekdays+) "Saturday")
(setf (gethash "Sun" +weekdays+) "Sunday")
(setf (gethash "Mon" +weekdays+) "Monday")
(setf (gethash "Tue" +weekdays+) "Tuesday")
(setf (gethash "Wed" +weekdays+) "Wednesday")
(setf (gethash "Thu" +weekdays+) "Thursday")
(setf (gethash "Fri" +weekdays+) "Friday")
(defparameter +months+ (make-hash-table :test 'equal))
(setf (gethash "Jan" +months+) "January")
(setf (gethash "Feb" +months+) "Febuary")
(setf (gethash "Mar" +months+) "March")
(setf (gethash "Apr" +months+) "April")
(setf (gethash "May" +months+) "May")
(setf (gethash "Jun" +months+) "June")
(setf (gethash "Jul" +months+) "July")
(setf (gethash "Aug" +months+) "August")
(setf (gethash "Sep" +months+) "September")
(setf (gethash "Oct" +months+) "October")
(setf (gethash "Nov" +months+) "November")
(setf (gethash "Dec" +months+) "December")

;;; Parse NOAA's unique time format into universal time and speakable
;;; formats.
;;; NOAA web pages have dates in this format: 715 PM EDT Sat May 16 2020
  
(defparameter +noaatime+
  "^(\\d+) (AM|PM) \\w+ \\w+ (\\w+) (\\d+) (\\d+)")

(defun noaa-time (line)
  (ppcre:register-groups-bind
   (('parse-integer hrs)
    ampm mon
    ('parse-integer day)
    ('parse-integer year))
   (+noaatime+ line :sharedp T)
   (let* ((hour (floor (/ hrs 100)))
	  (minute (mod hrs 100))
	  (hr24 (if (equal ampm "PM") (+ 12 hour) hour)))
     (cl-date-time-parser:parse-date-time
	    (format NIL "~d~2,'0d ~a ~d ~d" hr24 minute mon day year)))))

(defun get-tropical ()
  "Get an Atlantic Tropical Storm forecast."
  (let* ((lines (web-fetch "https://www.nhc.noaa.gov/gtwo.php"))
	 (uni 0)
	 (temp)
	 (text NIL))
    ;; Now remove any lines that are not necessary for speech output.
    (dolist (line lines)
      (cond
	;; Separate out the date line
	((setf temp (noaa-time line)) (setf uni temp))
	;; NNNN marks the end of a message
	((equal line "NNNN") (return))
	;; Ignore the forecaster's name
	((ppcre:scan "^Forecaster" line) (return))
	((equal line "&amp;&amp;") (return))
	;; Anything else goes into the result.
	((> uni 0) (push line text))
	(T T)))
    (values uni (agu:string-from-list (nreverse text)))))

(defun wx-repeat ()
  (agm:db-start)
  (let ((old (agm:get-info "ex-tropical")))
    (when old
      (setq *last* (car old))
      (agu:term
       "At ~a the National Weather Service reported~%~a~% "
       (speakable-time (car old))
       (cdr old))))
  (agm:db-commit))

;;;; The background operation to check for new forecasts from
;;;; time to time.
(defun wx-tropical ()
  (multiple-value-bind (date text) (get-tropical)
    ;; Ignore reports older than one day.
    (when (< (age date) 86400)
      (agu:term "uni |~a|~%" date)
      (if (equal date *last*)
	(agu:term
	 "There have been no tropical weather updates since ~a.~%"
	 (speakable-time *last*))
	;; We have a new report!  Remember it.
	(progn
	  (setq *last* date)
	  (save-wx "wx-tropical" date text)
	  (wx-repeat)))))

  ;; Schedule the next report 4 hours ahead.
  (agu:sked-later 14400 #'wx-tropical))
