;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Here is the function that can generate any number of sentences
;;;; with proper toki pona syntax.  These can be used for training
;;;; a language model.
(in-package :AGP)

(defparameter +percent-recursive+ 30)
(defun make-text (maxcount)
  (let ((genrules (make-hash-table :size 50))
	(funwords (words-by-fn))
	(topgoals NIL)
	(corpus (open
	  (format NIL "~a/corpus.txt" AGC:+data-directory+)
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

    (format T "Top goals: ~a~%" topgoals)
    ;; Now randomly walk the rules backwards, starting with one
    ;; of the top goals.
    (loop for n from 1 below maxcount
       do
	 (labels
	     ((pick-from-list (alternatives)
		(nth (random (length alternatives))
		     alternatives ))
	      (walk (start)
		(let ((in-rules (gethash start genrules))
		      (in-words (gethash start funwords)))
		  (cond
		    ((null in-rules)
			(let ((leaf (pick-from-list in-words)))
			  (format corpus " ~a" leaf)))
		    ((null in-words)
		     (let ((use-rule (pick-from-list in-rules)))
		       (walk (rule-left use-rule))
		       (walk (rule-right use-rule))))
		    (T (if
			(< (random 100) +percent-recursive+)
			(let ((use-rule (pick-from-list in-rules)))
			  (walk (rule-left use-rule))
			  (walk (rule-right use-rule)))
			(let ((leaf (pick-from-list in-words)))
			  (format corpus " ~a" leaf))))))))
	   (walk (pick-from-list topgoals)))
	 (format corpus ".~%")
	 )
  (close corpus)))
