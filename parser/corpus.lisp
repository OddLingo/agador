;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Here we can generate any number of sentences with proper toki
;;;; syntax, though they might be semanticaly meaningless.  These
;;;; can be used for training a language model.
(in-package :AGP)

(defparameter +percent-recursive+ 80)

(defun make-text (maxcount outfile &optional (period NIL))
  (format T "Writing ~D sentences to ~a~%" maxcount outfile)
  (let ((genrules (make-hash-table :size 50))
	(funwords (words-by-fn))
	(topgoals NIL)
	(corpus (open
	  outfile
	  :direction :output
	  :if-exists :supersede)))

    ;; Build a hash of rules keyed by result term.
    (dolist (rule AGF::+all-rules+)
      (destructuring-bind (lfn rfn rslt &optional act)
	  rule
	(let
	    ((newrule
	       (make-instance 'agp::rule
			  :left lfn :right rfn
			  :result rslt :action act))
	     (oldrules (gethash rslt genrules)))

	  ;; Remember the top goals
	  (when act (push rslt topgoals))

	  ;; Add new rule to the list of all with same result term
	  (setf (gethash rslt genrules)
		(if oldrules
		    (push newrule oldrules)
		    (list newrule)))
	  )))

    ;; Generate all the sentences.
    (loop for n from 1 to maxcount
       do
	 (let ((word-count 0))
	   (labels
	     ((pick-from-list (alternatives)
		(nth (random (length alternatives))
		     alternatives ))

	      (emit (w)
		(when (> word-count 0) (format corpus " "))
		(incf word-count)
		(format corpus w))

	      (deeper (d)
		(< (random 100) (ceiling (/ +percent-recursive+ d))))
	      
	      (walk (start &optional (depth 1))
		(let ((in-rules (gethash start genrules))
		      (in-words (gethash start funwords)))
		  (cond
		    ((null in-rules)
			(emit (pick-from-list in-words)))
		    ((null in-words)
		     (let ((use-rule (pick-from-list in-rules)))
		       (walk (rule-left use-rule) (1+ depth))
		       (walk (rule-right use-rule) (1+ depth))))
		    (T (if
			(deeper depth)
			(let ((use-rule (pick-from-list in-rules)))
			  (walk (rule-left use-rule) (1+ depth))
			  (walk (rule-right use-rule) (1+ depth)))
			(emit (pick-from-list in-words))))))))

	     ;; Now randomly walk the rules backwards, starting
	     ;; with one of the top goals.
	     (walk (pick-from-list topgoals)))

	   ;; Put a period at the end if requested.
	   (if period
	       (format corpus ".~%")
	       (format corpus "~%"))
	 ))
  (close corpus)))
