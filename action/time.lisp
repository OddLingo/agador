;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :AGA)

;;;; Dates and times are accumulated in this structure, so be converted
;;;; to universal time after all fields have bene filled in.
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
    (setf (gethash (car kv) (cdr kv)))))

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
;;;; like "AT nnnnnnn" where the number is in universal-time form.  That
;;;; is the form that would get 'remembered'.
;;;;
;;;; The applicable grammer fragments are:
;;;;   (DATEP (PREP (DATE MONTH NUMBER)) with PREP="ON" -> set month + day
;;;;   (TIMEP (PREP NUMBER)) with PREP="AT" -> set hour + minute
;;;;   (TIMEP (PREP NUMBER)) with PREP="IN" -> set year

;;; Which preposition is used is a clue as to which part of a time
;;; is being described.
(defun prep-name (pr)
  "Get the name of a preposition"
  (declare (type ppair pr))
  (let* ((prep (agp:word-at pr 'AGF::PREP)))
    (if prep (agc:spelled pr) NIL)))

(defun extract-month (pr dt)
  "Set a date from a MONTH NUMBER pair"
  (declare (type (ppair pr) (datetime dt)))
  (let* ((mname (spell (left pr)))
	 (mnum (gethash +month-numbers+ mname)))
    (when (< mnum (month dt))
      (incf (year dt)))
    (setf (month dt) mnum)
    dt))

(defun extract-year (n dt)
  "Set the year part of a date from a value NNNN"
  (declare (type (pnumb pr) (datetime dt)))
  (setf (year dt) (agc:value n))
  dt)

(defun extract-time (n dt)
  "Set a time from a numeric value HHMM"
  (declare (type (pnumb n) (datetime dt)))
  (let* ((v (agc:value n))
	 (hr (floor (/ v 100)))
	 (min (mod v 100)))
    (setf (hour dt) hr)
    (setf (minute dt) min)
    dt))

;;; Each prepositional phrase can set a subset of the fields in
;;; a date-time.
(defun analyze-prep (pr dt)
  "Analyze a preopostional phrase for time expressions"
  (let ((which (prep-name pr))
	(obj (agc:right pr)))
    (cond
      ((equal which "ON") (extract-date obj dt))
      ((equal which "IN") (extract-year obj dt))
      ((equal which "AT") (extract-time obj dt))
      (T NIL))))

