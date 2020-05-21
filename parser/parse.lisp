;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGP)

(defvar *seq*)

;; Given a term, find all its left adjacent terms
(defun minus1 (x) (- x 1))
(defun left-adjacent (rterm)
  (let ((npos (minus1 (term-lpos rterm))))
    (if (< npos 0) NIL (elt *right* npos))
    )
  )

(defun nseq () (setq *seq* (+ *seq* 1)))

(defun check-match (lt rt r)
  (if (eq (rule-left r) (agc:term-fn lt))
      (join lt rt (rule-result r) (action r)))
  )
	
;; Apply a list of rules to a list of left side candidates.  This is
;; not as bad as it looks because the list of rules has already been
;; limited to those with the correct right side term, and the candidates
;; are only those terms immediatly adjacent in the utterance.
;; This recurses through join so we declare that forward.
(declaim (ftype (function (agc:term agc:term SYMBOL SYMBOL) t) join))
(defun apply-rules (rt rules neighbors)
  (mapc (lambda (lt)
	  (mapc (lambda (r) (check-match lt rt r)) rules)
	  )
	neighbors
	)
  )

;; Consider what rules might apply to a new term, assuming that this
;; term would be on the right side of the rule.
(defun consider (rt)
  (let ((rules (rules-for (agc:term-fn rt)))
	(neighbors (left-adjacent rt)))
    (apply-rules rt rules neighbors)
    )
  NIL)

;; Join two adjacent terms into a pair that spans both terms.
;; This recursively then considers additional rules for the new pair.
(defun join (lt rt fn act)
  "Join two adjacent terms"
  (let ((np (make-instance 'ppair
			   :fn fn :seq (nseq)
			   :left lt :right rt
			   :action act))
	)
    (push np (elt *right* (term-rpos np)))
    (consider np)
    )
  NIL
  )

;; Add a new word to the current utterance being analyzed.
;; A postulated word might be any of the non-functional types,
;; but we mark it as a guess to be verified later by setting
;; the 'uncertainty' to 1.
(defun accept-word (wordpair)
  "Add a word to the sentence and look for matches"
  (let* ((pos (length *right*))
	 (spell (car wordpair))
	 (fn (cdr wordpair))
	 (rt (make-instance 'pusage
			    :spelled spell :fn fn
			    :lpos pos :rpos pos
			    :seq (nseq)
			    )))

    ;; Take this opportunity to learn new words from Julius.
;;    (if (null (get-word spell))
;;	(put-word spell (list fn)))
	      
    ;; Create an empty entry at the right end of the sentence.
    (vector-push-extend () *right*)
    ;; The new word is the term in this position
    (push rt (elt *right* pos))
    ;; Look at its left context.
    (consider rt)
    )
  )

(defun see-word (spell)
  "Add a word to the sentence and look for matches"
  (let* ((funs (agm:get-word spell))
	 (lfuns (if (null funs)
		    '(AGF::ADJ AGF::NOUN AGF::VERB AGF::ADV)
		    funs))
	 (pos (length *right*)))

    ;; Create an empty entry at the right end of the sentence.
    (vector-push-extend () *right*)

    (if (null funs)
	(agu:term  "  Guessing about '~a'~%" spell))

    ;; Create a USAGE for each potential grammatical function of
    ;; this word.  These are just internal to the parser and not
    ;; saved until a complete parse is accepted.
    (mapc
     (lambda (f)
       (let
	   ((rt (make-instance 'pusage
	       :spelled spell :fn f
	       :lpos pos :rpos pos
	       :seq (nseq) :unc (if (null funs) 1 0) )))
	 (push rt (elt *right* pos))
	 (consider rt)))
     lfuns)
    ))

;; This needs to be called before each parser invocation.
(defun init-parse ()
  "Initialize the parser"
  (setq *right*  (make-array 10 :fill-pointer 0 :adjustable t ))
  (setq *top* NIL)
  (setq *seq* 0)
  )

;; Remember all the guessed words used in an accepted parser output.
;; There might be none, but non-zero uncertainty values will lead
;; us to them.
(defgeneric seek-guesses (pterm))
(defmethod seek-guesses ((u pusage))
  (if (> (term-unc u) 0)
      (agm:put-word
       (agc:spelled u)
       (list (agc:term-fn u))))
      )
(defmethod seek-guesses ((p ppair))
  (if (> (term-unc p) 0)
      (block recurse
	(seek-guesses (agc:left p))
	(seek-guesses (agc:right p))
       ))
  )

;; Set *top* to all pairs that span the entire input string.
(defun choose-top ()
  (let ((rt (elt *right* (minus1 (length *right*)))))
    (setq *top*
	  (remove-if
	   (lambda (x) (> (term-lpos x) 0))
	   rt))
    ))

;; Remember any new words as well as what was said.
(defun learn (best)
  (let ((dothis (action best)))
    (if dothis
	(funcall dothis best)
	(agu:term "Nothing to do~%")
	)
    )
  )

;; If there is exactly one satisfactory solution, we can learn from it
;; or act on it.
(defun judge ()
  (agu:clear)
  (let ((nsoln (length *top*)))
    (cond ((= 0 nsoln)
	   (agu:term  "No satisfactory solution found~%")
	   (print-all)
	   NIL
	   )

	  ((= 1 nsoln)
	   ;; Exactly one - we go with it.
	   (let ((best (car *top*)))
	     (agu:use-term)
	     (agu:clear)
	     (agu:setxy 1 (paint-parse best))
	     (agu:set-scroll)
	     (finish-output)
	     (agu:release-term)
;	     (if (y-or-n-p "-- Is this correct?")
		 (learn best)
;		 NIL)
	     )
	   )

	  (T
	   (agu:term  "There are ~d solutions~%" nsoln)
	   (let ((topy 2))
	     (dolist (sol *top*)
	       (setf topy (paint-parse sol 2 topy)))
	     )
	   NIL
	   )
	  ))
  )

;; Parse a list of words
(defun parse-words (wds)
  (init-parse)
  ;; Feed each word to the parser.
  (mapc 'see-word wds)
  ;; Find the parses that span the entire input.
  (choose-top)
  ;; Detect a single useful result.
  (judge)
  )

(defun parse-string (s)
  (parse-words (agu:words-from-string s)))

(defun parse-file-line (fi)
  (parse-words (agu:words-from-file fi))
  )

;; Parse a list of words with functions already assigned.
(defun parse-msg (words)
  (init-parse)
  (mapc 'accept-word words)
  (choose-top)
  (judge)
  )

(defvar *parser-inbox*)

(defun parse (words)
  (sb-concurrency:send-message *parser-inbox* words)
  )

;; Load the grammar rules and start the grammar analysis thread.
(defun start-parser (lang)
  (init-rules lang)
  (setq *parser-inbox*
	(make-instance 'agu:mbx-server
		       :name "Parser"
		       :actor 'parse-msg))
  )
