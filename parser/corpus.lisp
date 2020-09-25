;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Here we can generate any number of sentences with proper
;;;; syntax, though they might be semanticaly meaningless.
(in-package :AGP)

(defparameter +percent-recursive+ 80)

;;; Build a hash of rules keyed by result term.  Each entry
;;; is a list of all rules with the same result.  We do not
;;; do this at compile time because make-text is only used
;;; during recognition training.
(defun invert-rules (genrules topgoals)
  (dolist (old-key (alexandria:hash-table-keys *rules*))
    (let ((rules (gethash old-key *rules*)))
      (dolist (r rules)
	(let*
	    ((rslt (rule-result r))
	     (oldrules (gethash rslt genrules)))

	  ;; Remember the top goals
	  (when (has-test r 'AGF::FINAL)
	    (push r topgoals))

	  ;; Add new rule to the list of all with same result term
	  (setf (gethash rslt genrules)
		(if oldrules
		    (push r oldrules)
		    (list r))))))))
    
(defun pick-from-list (alternatives)
  "Select a random element from a list"
  (let ((num (length alternatives)))
    (if num
	(nth (random num) alternatives )
	(log:error "Pick from no alternatives"))))

(defun pick-unique (alternatives previous)
  (when (and (not (null previous))
	     (< (length alternatives) 2))
    (log:warn "Want unique from short list ~a" alternatives)
    (return-from pick-unique (car alternatives)))
  (loop for choice = (pick-from-list alternatives)
     when (not (equal (agc:spelled choice)
		      (agc:spelled previous)))
     do (return-from pick-unique choice)))

(defun deeper (depth r)
  "Decide when to go deeper in the grammar"
  (declare (ignore r))
  (< (random 100)
     (ceiling (/ +percent-recursive+ depth))))

(defun make-text (maxcount outfile &optional (period NIL))
  "Generate a text corpus"
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

    (invert-rules genrules topgoals)

    ;; Generate all the sentences.
    (loop for n from 1 to maxcount
       do
	 (setf word-count 0)
	 (labels
	      ((emit (w)
		"Insert space before all but first word"
		(when (> word-count 0) (format corpus " "))
		(incf word-count)
		(format corpus (string-upcase w)))

	       (pick-word (words &optional (previous NIL))
		 "Choose a random word from a list"
		 (if words
		     (emit (pick-unique words previous))
		     (log:error "Picking from empty word list~%")))

	       (pick-rule (depth rules)
		 "Choose a random rule from a list"
		 (if rules
		     (walk (pick-from-list rules) (1+ depth))
		     (log:error "Picking from empty rules list~%")))

	       (probe (r side fn depth &optional (previous NIL))
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
			((null rules) (pick-word words previous))
			((null words) (pick-rule depth rules))
			(T (if
			    (deeper depth r)
			    (pick-rule depth rules)
			    (pick-word words previous)))))))

	      (walk (start &optional (depth 1))
		"Recursively descend grammar, making random choices."
		(let* ((lfn (rule-left start))
		       (rfn (rule-right start))
		       (lchoice (probe start 'AGF::LEFT lfn depth)))
		  (probe start 'AGF::RIGHT rfn depth lchoice)))
	      ) ;; End of local functions

	   ;; Now randomly walk the rules down from one of
	   ;; the top goals.
	   (walk (pick-from-list topgoals))) ;; End of labels

         ;; End of line at end of each sentence.
	 (format corpus "~%")) ;; End loop making sentences.
    (close corpus)))
