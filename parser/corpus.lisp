;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Here we can generate any number of sentences with proper toki
;;;; syntax, though they might be semanticaly meaningless.  These
;;;; are used for training both acoustic and language models.
(in-package :AGP)

(defparameter +percent-recursive+ 80)

(defun make-text (maxcount outfile &optional (period NIL))
  (declare (ignore period))
  (declare (optimize (debug 3) (speed 0)))
  (format T "Writing ~D sentences to ~a~%" maxcount outfile)
  (let ((genrules (make-hash-table :size 50))
	(funwords (words-by-fn))
	(topgoals NIL)
	(word-count 0)
	(corpus (open
	  outfile
	  :direction :output
	  :if-exists :supersede)))

    ;; Build a hash of rules keyed by result term.  Each entry
    ;; is a list of all rules with the same result.  We do not
    ;; do this at compile time because make-text is only used
    ;; during recognition training.
    (dolist (old-key (alexandria:hash-table-keys *rules*))
      ;;? This only considers rules by right term.
      (let ((rules (gethash old-key *rules*)))
	(dolist (r rules)
	  (let* ((rslt (rule-result r))
		(oldrules (gethash rslt genrules)))

	    ;; Remember the top goals
	    (when (has-test r 'AGF::FINAL)
	      (push r topgoals))

	    ;; Add new rule to the list of all with same result term
	    (setf (gethash rslt genrules)
		(if oldrules
		    (push r oldrules)
		    (list r)))
	    ))))
    
    ;; Generate all the sentences.
    (loop for n from 1 to maxcount
       do
	 (setf word-count 0)
	 (labels
;;	     (declaim (ftype (function (rule integer) t) walk))
	     ((pick-from-list (alternatives)
		"Select a random element from a list"
		(nth (random (length alternatives))
		     alternatives ))

	      (emit (w)
		"Insert space before all but first word"
		(when (> word-count 0) (format corpus " "))
		(incf word-count)
		(format corpus (string-upcase w)))

	      (pick-word (words)
		"Choose a random word form a list"
		(if (null words)
		    (format T "Picking from empty word list~%")
		    (emit (pick-from-list words))))
	      
	      (pick-rule (depth rules)
		"Choose a random rule from a list"
		(if (null rules)
		    (format T "Picking from empty rules list~%")
		    (walk (pick-from-list rules) (1+ depth))))

	      (deeper (depth r)
		"Decide when to go deeper in the grammar"
		(declare (ignore r))
		(< (random 100)
		   (ceiling (/ +percent-recursive+ depth))))

	      (probe (r side fn depth)
		(if (has-test r side)
		    ;; Sometimes always take a rule.
		    (pick-rule depth (gethash fn genrules))

		    ;; Sometimes choose between a word and a rule.
		    (let (
			  ;; Get all the choices at this point.
			  (rules (gethash fn genrules))
			  (words (gethash fn funwords)))
		      
		      (cond
			;; No matching rules, so use words.
			((null rules) (pick-word words))
			((null words) (pick-rule depth rules))
			(T (if
			    (deeper depth r)
			    (pick-rule depth rules)
			    (pick-word words)))))))

	      (walk (start &optional (depth 1))
		"Recursively walk down the grammar, making random decisions."
		(let* ((lfn (rule-left start))
		       (rfn (rule-right start)))
		  (probe start 'AGF::LEFT lfn depth)
		  (probe start 'AGF::RIGHT rfn depth)))
	      ) ;; End of local functions

	   ;; Now randomly walk the rules down from one of
	   ;; the top goals.
	   (walk (pick-from-list topgoals)))

         ;; End of line at end of each sentence.
	 (format corpus "~%")

	 ) ;; End loop making sentences.
    (close corpus)))

