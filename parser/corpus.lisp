;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Here we can generate any number of sentences with proper
;;;; syntax, though they might be semanticaly meaningless.
(in-package :AGP)

(defparameter +percent-recursive+ 80)

;;; Build a hash of rules keyed by result term.  Each entry
;;; is a list of all rules with the same result.  We do not
;;; do this at compile time because make-text is only used
;;; during recognition training.
(defun invert-rules ()
  (let ((topgoals NIL)
	(genrules (make-hash-table :size 30)))
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
		      (list r)))))))
  (values topgoals genrules)))

(defun pick-from-list (alternatives)
  "Select a random element from a list"
  (declare (optimize (debug 3) (speed 0)))
  (let ((num (length alternatives)))
    (cond
      ((= num 1) (car alternatives))
      ((> num 1) (nth (random num) alternatives ))
      (T (break "Pick from no alternatives")))))

(defun pick-unique (alternatives previous)
  (when (and (not (null previous))
	     (< (length alternatives) 2))
    (log:warn "Want unique from short list ~a" alternatives)
    (return-from pick-unique (car alternatives)))
  (loop for choice = (pick-from-list alternatives)
     when (not (equal choice previous))
     do (return-from pick-unique choice)))

(defun deeper (depth r)
  "Decide when to go deeper in the grammar"
  (declare (ignore r))
  (< (random 100)
     (ceiling (/ +percent-recursive+ depth))))

(defun make-text (maxcount outfile &optional (period NIL))
  "Generate a text corpus"
  (declare (ignore period))
  (format T "Writing ~D sentences to ~a~%" maxcount outfile)
  (multiple-value-bind
	(topgoals genrules)
      (invert-rules)
    (let ((funwords (words-by-fn))
	  (word-count 0)
	  (corpus (open
		   outfile
		   :direction :output
		   :if-exists :supersede)))

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
		  (if previous
		      (case (type-of previous)
			(:RULE
			 (emit (pick-from-list words)))
			(otherwise
			 (emit (pick-unique words previous))))
		      (emit (pick-from-list words))))

		(pick-rule (depth rules &optional (previous NIL))
		  "Choose a random rule from a list"
		  (declare (type integer depth)
			   (optimize (debug 3)(speed 1)))
		  (if previous
		      (case (type-of previous)
			(:RULE
			 (walk (pick-unique rules previous) (1+ depth)))
			(otherwise
			 (walk (pick-from-list rules) (1+ depth))))
		      (walk (pick-from-list rules) (1+ depth))))

		(probe (r side fn depth &optional (previous NIL))
		  (declare (type RULE r)
			   (type integer depth) (type symbol side fn))
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
			  ((null words) (pick-rule depth rules previous))
			  (T (if
			      (deeper depth r)
			      (pick-rule depth rules previous)
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
	   (unless topgoals (break "No top goals"))
	   (walk (pick-from-list topgoals))) ;; End of labels

         ;; End of line at end of each sentence.
	 (format corpus "~%")) ;; End loop making sentences.
    (close corpus))))
