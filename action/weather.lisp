;;;; Obtains weather information from NHC TWO (Tropical Weather Outlook)
;; See https://lispcookbook.github.io/cl-cookbook/web-scraping.html

(in-package :AGA)
(defparameter +map+ "https://www.nhc.noaa.gov/xgtwo/two_atl_5d0.png")
(defvar *lastnhc* NIL)
(defvar *nhctext* NIL)

;; Regex patterns for extracing text from the NHC web page.
;; (defparameter +articlestart+ "<div class='textproduct'><pre>")
(defparameter +datestart+ "^\\d+ AM|PM EDT|EST")
(defparameter +textend+ "^</pre>")

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

;; 715 PM EDT Sat May 16 2020
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

(defun get-tropical ()
  (let* ((lines (web-fetch "https://www.nhc.noaa.gov/gtwo.php"))
	 (date NIL)
	 (result NIL)
	 )
    (dolist (line lines)
      (cond
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
		       hour minute ampm week month day))
	    )))
	((equal line "NNNN") (return))
	((ppcre:scan "^Forecaster" line))
	((equal line "&amp;&amp;") (return))
	((not (null date)) (push line result))
	(T T)))
    (values date (agu:string-from-list (nreverse result)))
    )
    )

(defun wx-tropical ()
  (multiple-value-bind (date text) (get-tropical)
    (if (equal date *lastnhc*)
	(agu:term "No tropical weather updates since ~a~%" *lastnhc*)
	(progn
	  (setq *lastnhc* date)
	  (setq *nhctext* text)
	  (agu:term
	   "At ~a the National Weather Service reported as follows. ~a~% "
	   date text)
	  )
	)
    ))


;; ("" "For the North Atlantic...Caribbean Sea and the Gulf of Mexico:" ""
;;  "The National Hurricane Center is issuing advisories on Tropical "
;;  "Depression One, located over the western Atlantic Ocean off the "
;;  "east-central coast of Florida." ""
;;  "Tropical cyclone formation is not expected during the next 5 days." ""
;;  "Routine issuance of the Tropical Weather Outlook will resume on "
;;  "June 1, 2020.  Until then, Special Tropical Weather Outlooks will "
;;  "be issued as conditions warrant." "" "&amp;&amp;"
;;  "Public Advisories on Tropical Depression One are issued under "
;;  "WMO header WTNT31 KNHC and under AWIPS header MIATCPAT1."
;;  "Forecast/Advisories on Tropical Depression One are issued under WMO "
;;  "header WTNT21 KNHC and under AWIPS header MIATCMAT1." "" "$$"
;;  "Forecaster Berg" "NNNN" "")
