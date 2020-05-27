;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :AGA)

(defparameter +day-names+
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter +month-names+
  '("January" "February" "March" "April" "May" "June"
    "July" "august" "September" "October" "november" "December"))

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
	    (nth month +month-names+)
	    day)))

(defun saytime ()
  (ags:say (format NIL "It is now ~a"
		   (speakable-time (get-universal-time)))))

