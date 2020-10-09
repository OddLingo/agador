;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; Managing the console window.

(in-package :AGU)

(defun gfun-from-string (s) (intern s :AGF))

(defun words-from-string (s)
  (cl-ppcre:split "[ \-\\[\\]\\+_]" (string-trim " " s)))
  
;;; The inverse of words-from-string.
(defun string-from-list (l) (format NIL "~{~a~^ ~}" l))

;;; Initializes hash tables at compile time.
(defun init-hash (h v)
  (dolist (kv v)
    (setf (gethash (car kv) h) (cdr kv))))

;;; Read one line from a file and split it into words.
;;; Returns NIL at end-of-file.
(defun words-from-file (file)
  (let ((line (read-line file NIL)))
    (if (null line) NIL
	(words-from-string line))))

