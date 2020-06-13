;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are functions that convert spoken numeric values into
;;;; actual numbers, so that "fifteen" becomes 15.  They are also
;;;; used by date-time functions so that "January twenty third"
;;;; bcecomes a universal time value.
(in-package :AGP)

(defvar *current-number* NIL)
(defvar *last-digit* 0)

(defun number-reset ()
  (setq *last-digit* 0)
  (setq *current-number* NIL))

;;; This hash table maps the names of numbers to their values.
(defparameter +nummap+ (make-hash-table :test 'equal :size 30))
(dolist (num
  '(("ZERO" . 0) ("OH" . 0) ("ONE" . 1) ("TWO" . 2) ("THREE" . 3) ("FOUR" . 4)
    ("FIVE" . 5) ("SIX" . 6) ("SEVEN" . 7) ("EIGHT" . 8) ("NINE" . 9)
    ("TEN" . 10) ("ELEVEN" . 11) ("TWELVE" . 12) ("THIRTEEN" . 13)
    ("FOURTEEN" . 14) ("FIFTEEN" . 15) ("SIXTEEN" . 16)
    ("SEVENTEEN" . 17) ("EIGHTEEN" . 18) ("NINETEEN" . 19)
    ("TWENTY" . 20) ("THIRTY" . 30) ("FOURTY" . 40) ("FIFTY" . 50)))
  (setf (gethash (car num) +nummap+) (cdr num)))

;;; Find the power of ten of a number.
(defun number-factor (n)
  (cond
    ((< n 10) 1)     ;; 0-9
    ((< n 100) 10)   ;; 10-90
    (T 100)))

;;; Add successive digits so that "twenty seven"
;;; becomes 27 and "eight fifteen" becomes 815.
(defun number-shift (newval)
  (let* ((lf (number-factor *last-digit*))
	 (rf (number-factor newval))
	 (ef (cond
	       ((> rf lf) rf)   ;; 815
	       ((< rf lf) lf)   ;; 23
	       (T (* 10 rf))))) ;; 2019
    (setf (agc:nvalue *current-number*)
	(+ newval
	   (* ef (agc:nvalue *current-number*))))))

;;; Add the value represented by the 'name' of a digit to the current
;;; accumulating value.  "one two three" will become 123 and
;;; "thirty seven" will become 37.
(defun number-add (spell)
  (let ((digval (gethash spell +nummap+)))
    (number-shift digval)
    (setq *last-digit* digval)))

