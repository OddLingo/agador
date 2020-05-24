;;;; Obtains weather information from NHC TWO (Tropical Weather Outlook)
;; See https://lispcookbook.github.io/cl-cookbook/web-scraping.html

(in-package :AGA)
(defparameter +map+ "https://www.nhc.noaa.gov/xgtwo/two_atl_5d0.png")
(defvar *lastnhc* NIL)
(defvar *nhctext* NIL)

;;; Regex patterns for extracing text from the NHC web page.
(defparameter +datestart+ "^\\d+ AM|PM EDT|EST")
(defparameter +textend+ "^</pre>")

;;; General purpose function for fetching a web page.
(defun web-fetch (url)
  (let* ((request (dex:get url))
	 (parsed (lquery:$ (initialize request)))
	 (dom (lquery:$ parsed "div" ".textproduct" "pre"))
	 (texts (lquery:$ dom (serialize)))
	 (text (elt texts 0))
	 )
    (cl-utilities:split-sequence '#\Linefeed text)
    )
  )

;;; NOAA web pages have dates in this format: 715 PM EDT Sat May 16 2020
(defparameter +noaatime+
  "^(\\d+) (AM|PM) \\w+ (\\w+) (\\w+) (\\d+) \\d+")

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

;;;; Use web-fetch to get an Atlantic Tropical Storm forecast.
(defun get-tropical ()
  (let* ((lines (web-fetch "https://www.nhc.noaa.gov/gtwo.php"))
	 (date NIL)
	 (result NIL))
    ;; Now remove any lines that are not necessary for speech output.
    (dolist (line lines)
      (cond
	;; Separate out the date line
	((ppcre:register-groups-bind
	  (('parse-integer hrs)
	   ampm dow mon
	   ('parse-integer day))
	  (+noaatime+ line :sharedp T)
	  (let* (
		 (hour (floor (/ hrs 100)))
		 (minute (mod hrs 100))
		 (week (gethash dow +weekdays+))
		 (month (gethash mon +months+)))
	    (setf date (format NIL "~d:~2d ~a on ~a, ~a ~:R"
			       hour minute ampm week month day)))))

	;; NNNN marks the end of a message
	((equal line "NNNN") (return))
	;; Ignor ethe forecaster's name
	((ppcre:scan "^Forecaster" line))
	((equal line "&amp;&amp;") (return))
	;; Anything else goes into the result.
	((not (null date)) (push line result))
	(T T)))
    (values date (agu:string-from-list (nreverse result)))))

(defun repeat ()
  (agu:term
   "At ~a the National Weather Service reported~%~a~% "
   *lastnhc* *nhctext*))

;;;; The background operation to check for new forecasts from
;;;; time to time.
(defun wx-tropical ()
  (multiple-value-bind (date text) (get-tropical)
    (if (equal date *lastnhc*)
	(agu:term
	 "There have been no tropical weather updates since ~a.~%"
	 *lastnhc*)
	;; We have a new report!  Remember it.
	(progn
	  (setq *lastnhc* date)
	  (setq *nhctext* text)
	  (repeat))))

  ;; Schedule the next report 4 hours ahead.
  (agu:sked-later 14400 #'wx-tropical))
