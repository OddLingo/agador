;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :BTP)

;; A hash table of rules, keyed on the right hand term.
(defvar *rules* (make-hash-table :size 30))

;; Each entry is a list of rules with the same right term.  We
;; create the list the first time, and add to it thereafter.  When
;; trying out potential rules, we always know what the right side
;; is, so this speeds up the search.
(defun add-rule (lfn rfn rslt)
  (let ((newrule
	 (make-instance 'rule :left lfn :right rfn :result rslt))
	(oldrules (gethash rfn *rules*)))
    (setf (gethash rfn *rules*)
	  (if (null oldrules)
	      (list newrule)
	      (push newrule oldrules)))
    )
  )

;; Get a list of the rules with a specified right side term.
(defun rules-for (fn) (gethash fn *rules*))

;; Load the rules at startup.
(defun load-rules (fname)
  "Load rules from a file"
  (setq *rules* (make-hash-table :size 30))
  (with-open-file (stream fname)
    (loop for strs = (agu:words-from-file stream)
       while strs do
	 (let ((lfn (intern (car strs) :btf))
	       (rfn (intern (cadr strs) :btf))
	       (rslt (intern (caddr strs) :btf))
	       )
	   (add-rule lfn rfn rslt)
	   )
	 )
    )
   )

(defun init-rules ()
  (load-rules "data/rules.txt")
  )

;; Print all the rules
(defun print-rules ()
  (maphash
   #'(lambda (rtype rules)
       (format t "For ~a:" rtype)
       (mapc (lambda (r)
	       (progn
		 (format T "    ")
		 (print-object r t)
		 (terpri)
		 )
	       ) rules)
       (terpri)
       )
   *rules*))
       
