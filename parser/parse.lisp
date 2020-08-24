;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This is the Adjacency Parser.  It applies simple 3-term rules to
;;;; a sequence of words in order to discover the syntactic structure
;;;; of the sentence, much like "diagramming" in an English calss.
;;;; Unlike most parsers for programming languages, it can deal with
;;;; local ambiguities.

(in-package :AGP)

;;; Given a term, find all its left adjacent terms
(defun minus1 (x) (- x 1))
(defun left-adjacent (rterm)
  (let ((npos (minus1 (term-lpos rterm))))
    (if (< npos 0) NIL (elt *right* npos))))

;;; Apply a list of rules to a list of left side candidates.  This is
;;; not as bad as it looks because the list of rules has already been
;;; limited to those with the correct right side term, and the candidates
;;; are only those terms immediately adjacent to the left in the utterance.
;;; This recurses through join so we declare that forward.
(declaim (ftype (function (agc:term agc:term SYMBOL SYMBOL) t) join))
(defun apply-rules (rt rules neighbors)
  (dolist (lt neighbors)
    (dolist (r rules)
      (when (eq (rule-left r) (agc:term-fn lt))
	(join lt rt (rule-result r) (action r))))))

(defun consider (rt)
  "Consider what rules might apply to a new term, assuming that this
  term would be on the right side of the rule."
  (let ((rules (rules-for (agc:term-fn rt)))
	(neighbors (left-adjacent rt)))
    (apply-rules rt rules neighbors))
  NIL)

;;; Join two adjacent terms into a pair that spans both terms.
;;; This recursively then considers additional rules for the new pair.
(defun join (lt rt fn act)
  "Join two adjacent terms"
  (let ((np (make-instance 'ppair
			   :fn fn
			   :left lt :right rt
			   :action act)))
    (push np (elt *right* (term-rpos np)))
    (consider np))
  NIL)

;;; Add a new word to the current utterance being analyzed.
(defun accept-word (wordpair)
  "Add a word to the sentence and look for matches"
  (let* ((pos (length *right*))
	 (spell (car wordpair))
	 (fn (cdr wordpair)))
    (if (eq fn 'AGC::DIGIT)
	(progn
	  ;; Special case handling of numbers.
	  (unless *current-number*
	    ;; First of a run of digits.
	    (setq *current-number*
		  (make-instance 'pnumb :lpos pos :rpos pos))
	    (vector-push-extend () *right*)
	    (push *current-number* (elt *right* pos)))
	  ;; Process this digit.
	  (number-add spell))
	(progn
	  ;; If we were just handling a number, look for its
	  ;; rules first.
	  (when *current-number*
	    (consider *current-number*)
	    (number-reset))
	  ;; Now deal with the new non-digit word.
	  (vector-push-extend () *right*)
	  (let ((rt (make-instance 'pusage
			    :spelled spell :fn fn
			    :lpos pos :rpos pos
			    )))
	    (push rt (elt *right* pos))
	    (consider rt))))))

(defun see-word (spell)
  "Add a word to the sentence and look for matches"
  (let* ((funs (lookup spell))
	 (pos (length *right*)))

    ;; Create an empty entry at the right end of the sentence.
    (vector-push-extend () *right*)

    ;; Create a USAGE for each potential grammatical function of
    ;; this word.  These are just internal to the parser and not
    ;; saved until a complete parse is accepted.
    (dolist (f funs)
      (let (
	    (rt (make-instance 'pusage
	       :spelled spell :fn f
	       :lpos pos :rpos pos)))
	 (push rt (elt *right* pos))
	 (consider rt)))))

;;; This needs to be called before each parser invocation.
(defun init-parse ()
  "Initialize the parser"
  (setq *right*  (make-array 10 :fill-pointer 0 :adjustable t ))
  (setq *top* NIL))

;;; Set *top* to all pairs that span the entire input string.
;;; Hopefully there is just one, and it will have all local
;;; ambiguities dealt with.
(defun choose-top ()
  (let ((rt (elt *right* (minus1 (length *right*)))))
    (setq *top*
	  (remove-if
	   (lambda (x) (> (term-lpos x) 0))
	   rt))))

;;; Remember what was said.
(defun learn (best)
  (declare (type pterm best))
  (log:info "Recognized: ~a" (string-from-tree best))
  (let ((dothis (action best)))
    (if dothis
	(handler-case
	    (funcall dothis best)
	  (error (e)
	    (log:error "~a" e)))
	(agu:term "Nothing to do~%"))))

;;; If there is exactly one satisfactory solution, we can learn from it
;;; or act on it.
(defun judge ()
  (agu:clear)
  (let ((nsoln (length *top*)))
    (cond ((= 0 nsoln)
	   (log:warn "No satisfactory solution found~%")
	   (print-all)
	   NIL)

	  ((= 1 nsoln)
	   ;; Exactly one - we go with it.
	   (let ((best (car *top*)))
	     (paint-parse best)
	     (learn best)))

	  (T
	   (agu:term  "There are ~d solutions~%" nsoln)
	   (let ((topy 2))
	     (dolist (sol *top*)
	       (setf topy (paint-parse sol 2 topy))))
	   NIL))))

;;; Parse a list of words
(defun parse-words (wds)
  (init-parse)
  ;; Feed each word to the parser.
  (dolist (w wds) (see-word w))
  ;; Find the parses that span the entire input.
  (choose-top)
  ;; Detect a single useful result.
  (judge))

(defun parse-string (s)
  (parse-words (agu:words-from-string s)))

(defun parse-file-line (fi)
  (parse-words (agu:words-from-file fi))
  )

(defvar *parser-inbox*)

(defun parse (words)
  (sb-concurrency:send-message *parser-inbox* words))

(defun start-parser ()
  "Initialize parser structures."
  (setq *parser-inbox*
	(make-instance 'agu:mbx-server
		       :name "Parser"
		       :actor 'parse-words)))
