;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :AGA)

(defconstant +day-names+
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defconstant +month-names+
  '("January" "February" "March" "April" "May" "June"
    "July" "august" "September" "October" "november" "December"))

;; Make the text of a time announcement.
(defun daystring ()
  (multiple-value-bind
    (second minute hour day month year day-of-week dst-p tz)
    (get-decoded-time)
    (format NIL "It is now ~2,'0d:~2,'0d on ~a, ~a ~:R."
	    hour minute (nth day-of-week +day-names+)
	    (nth month +month-names+) day))
  )

(defun saytime () (ags:say (daystring)))

