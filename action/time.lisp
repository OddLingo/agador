;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :AGA)


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
	    (nth (1- month) +month-names+)
	    day)))

(defun saytime ()
  (ags:say (format NIL "It is now ~a"
		   (speakable-time (get-universal-time)))))

