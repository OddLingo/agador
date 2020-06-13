;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :AGA)

;;;; Dates and times are accumulated in this structure, to be converted
;;;; to universal time after all fields have bene filled in.  When
;;;; first created, fields are set to the current time.
(defclass datetime ()
  (
   (year :accessor year :type integer)
   (month :accessor month :type integer)
   (day :accessor day :type integer)
   (hour :accessor hour :type integer)
   (minute :accessor minute :type integer)
   ))

(defmethod initialize-instance :before ((dt datetime) &key)
  (multiple-value-bind
   (sec min hr dy mon yr)
      (get-decoded-time)
    (declare (ignore sec))
    (setf (year dt) yr)
    (setf (month dt) mon)
    (setf (day dt) dy)
    (setf (hour dt) hr)
    (setf (minute dt) min)))
	   
(defmethod to-universal ((dt datetime))
  "Convert finished date-time to universal seconds"
  (encode-universal-time
   0 (minute dt) (hour dt)
   (day dt) (month dt) (year dt)))

(defun init-hash (h v)
  (dolist (kv v)
    (setf (gethash (car kv) h) (cdr kv))))

(defparameter +day-names+
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter +month-names+
  '("January" "February" "March" "April" "May" "June" "July" "august"
    "September" "October" "november" "December"))

(defparameter +month-numbers+ (make-hash-table :test 'equal :size 12))
(init-hash
 +month-numbers+
 '(("JANUARY" . 1) ("FEBRUARY" . 2) ("MARCH" . 3) ("APRIL" . 4)
   ("MAY" . 5) ("JUNE" . 6) ("JULY" . 7) ("AUGUST" . 8)
   ("SEPTEMBER" . 9) ("OCTOBER" . 10) ("NOVEMBER" . 11) ("DECEMBER" . 12)))

(defun age (uni)
  "Compute the age of a message in seconds"
  (- (get-universal-time) uni))

;;;; Make the text of a time announcement.
(defun speakable-time (uni)
  (declare (fixnum uni))
  (multiple-value-bind
	(second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time uni)
    (declare (ignore second dst-p tz year)
	     (fixnum hour minute day month year))
    (format NIL "~d:~2,'0d ~a on ~a, ~a ~:R"
	    (if (> hour 12) (- hour 12) hour)
	    minute
	    (if (> hour 11) "PM" "AM")
	    (nth day-of-week +day-names+)
	    (nth (1- month) +month-names+)
	    day)))

(defun saytime ()
  (ags:say (format NIL "It is now ~a."
		   (speakable-time (get-universal-time)))))

;;;; Here we process time expressions in prepositional phrases, to convert
;;;; them into single universal-time integers.  So a sequence of phrases like
;;;; "ON JANUARY THIRTEENTH AT TWO THIRTY" becomes a single short phrase
;;;; "AT nnnnnnn" where the number is in universal-time form.  That
;;;; is the form that would get 'remembered'.
;;;;
;;;; The applicable grammer fragments are:
;;;;   (PP (PREP (DATE MONTH NUMBER)) with PREP="ON" -> set month + day
;;;;   (PP (PREP NUMBER)) with PREP="AT" -> set hour + minute
;;;;   (PP (PREP NUMBER)) with PREP="IN" -> set year
;;;;   (PP (PREP MONTH)) with PREP="IN" -> set first of month
;;;; These patterns are encoded in the +prep-rules+ list.

(defun analyze-date (pr dt)
  "Set a date from a MONTH NUMBER pair"
  (declare (type agp:ppair pr)
	   (type datetime dt))
  (let* ((mname (agc:spelled (agc:left pr)))
	 (mnum (gethash +month-numbers+ mname))
	 (dnum (agc:nvalue (agc:right pr))))
    (when (< mnum (month dt))
      (incf (year dt)))
    (setf (month dt) mnum)
    (setf (day dt) dnum)
    dt))

(defun analyze-month (mon dt)
  "Set a date given just a month name"
  (declare (type agp:pusage mon)
	   (type datetime dt))
  (let* ((mname (agc:spelled mon))
	 (mnum (gethash mname +month-numbers+)))
    ;; Bump to next year if month in the past
    (when (< mnum (month dt))
      (incf (year dt)))
    (setf (month dt) mnum)
    (setf (day dt) 1)
    dt))
  
(defun analyze-year (n dt)
  "Set the year part of a date from a value NNNN"
  (declare (type agp:pnumb n)
	   (type datetime dt))
  (setf (year dt) (agc:nvalue n))
  dt)

(defun analyze-time (n dt)
  "Set a time from a numeric value HHMM"
  (declare (type agp:pnumb n)
	   (type datetime dt))
  (let* ((v (agc:nvalue n))
	 (hr (floor (/ v 100)))
	 (min (mod v 100)))
    (setf (hour dt) hr)
    (setf (minute dt) min)
    dt))

;;; Here is a list of the patterns we look for.  The first term is
;;; the spelling of the preposition itself.  The second term is the
;;; gramatical function of the right side of the phrase.  The last
;;; term is the function to call to set datetime fields.  These phrases
;;; appear on the right side of an MSTMT 'modified statement' pair.
(defparameter +prep-rules+
'(("AT" 'AGF::NUMBER 'analyze-time)  ;; "at 1115"
  ("IN" 'AGF::MONTH 'analyze-month)  ;; "in January"
  ("BY" 'AGF::MONTH 'analyze-month)  ;; "by January"
  ("IN" 'AGF::NUMBER 'analyze-year)  ;; "in 2021"
  ("BY" 'AGF::DATE 'analyze-date)
  ("ON" 'AGF::DATE 'analyze-date)))  ;; "on July tenth"  

(defun prep-patterns (pp dt)
  "Look for patterns in the name of a preposition and its object type"
  (declare (type agp:ppair pp)
	   (type datetime dt))
  (let* ((pname (agc:spelled (agc:left pp)))
	(arg (agc:right pp))
	(fn (agc:term-fn arg)))
    (dolist (pattern +prep-rules+)
      (destructuring-bind
	    (rule-name rule-fun process)
	  pattern
	(when (and
	   (equal rule-name pname)
	   (eq rule-fun fn))
	  (format T "PP ~a~%" pattern)
	  (funcall process arg dt)
	  (return-from prep-patterns T))))
    NIL))		

;;; We need to sweep through a statement watching for prepositional
;;; phrases and analyzing each one for time information.
(defun scan-preps (start)
  (declare (type agp:ppair start))
  (let* ((dt (make-instance 'datetime)))
    (labels
	((pp-scan (node dt)
	   (declare (type agp:pterm node)
		    (type datetime dt))
	   (let ((nty (type-of node))
		 (nfn (agc:term-fn node)))
	     (cond
	       ((eq nfn 'AGF::PP) (prep-patterns node dt))
	       ((eq nty 'AGP:PPAIR)
		(progn
		  (pp-scan (agc:left node) dt)
		  (pp-scan (agc:right node) dt)))))))
      (pp-scan start dt))
    ;; The datetime is now fillled in.  Convert to standard seconds format.
    (to-universal dt)))

(defun time-check (start)
  (declare (type AGP:PPAIR start))

  ;; This only applies to the right side of MSTMT terms.
  (unless (eq (agc:term-fn start) 'AGC::MSTMT)
    (return-from time-check NIL))

  (let* ((top-right (AGC:RIGHT start))
	 (uni-time (scan-preps top-right)))
    (AGU:TERM "Sentence references ~a~%" (speakable-time uni-time))
    uni-time))

