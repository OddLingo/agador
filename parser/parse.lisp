;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This is the Adjacency Parser.  It applies simple 3-term rules to
;;;; a sequence of words in order to discover the syntactic structure
;;;; of the sentence, much like "diagramming" in an English class.
;;;; Unlike most parsers for programming languages, it can deal with
;;;; local ambiguities.

(in-package :AGP)

;;; Given a term, find all its left adjacent terms
(defun minus1 (x) (- x 1))
(defun left-adjacent (rterm)
  (let ((npos (minus1 (term-lpos rterm))))
    (if (< npos 0) NIL (elt *right* npos))))

;;; Approve a rule that has additional tests.  Any test
;;; returning NIL will block the join.
(defun approved (actions lt rt)
  "Check all special conditions"
  (dolist (act actions)
      (unless (approve-join act lt rt)
	(log:info "Join blocked by ~a" act)
	(return-from approved NIL)))
  T)

;;; Apply a list of rules to a list of left side candidates.
;;; This is not as bad as it looks because the list of rules
;;; has already been limited to those with the correct right
;;; side term, and the candidates are only those terms
;;; immediately adjacent to the left in the utterance.
;;; In addition, the 'action' functions have the ability
;;; to disallow certain applications.
(defun apply-rules (rt rules neighbors)
  (dolist (lt neighbors)
    (dolist (r rules)
      (when (eq (rule-left r) (agc:term-fn lt))
	(when (approved (action r) lt rt)
	  (join lt rt (rule-result r) (action r)))))))

(defun consider (rt)
  "Consider what rules might apply to a new term"
  (let ((rules (rules-for (agc:term-fn rt)))
	(neighbors (left-adjacent rt)))
    (apply-rules rt rules neighbors))
  NIL)

;;; Join two adjacent terms into a pair that spans both
;;; terms. This recursively then considers additional
;;; rules for the new pair.
(defun join (lt rt fn act)
  "Join two adjacent terms"
  (log:info "~a ~a ~a" lt rt fn)
  (let ((np (make-instance 'ppair
			   :fn fn
			   :left lt :right rt
			   :action act)))
    ;; Add the pair to the list for this position.
    (push np (elt *right* (term-rpos np)))
    ;; Look for rules that apply to the new pair.
    (consider np))
  NIL)

(defun see-word (spell)
  "Add a word to the sentence and look for matches"
  (let* ((funs (lookup spell))
	 (pos (length *right*))
	 (priority 100))

    ;; Create an empty entry at the right end of the sentence.
    (vector-push-extend () *right*)

    ;; Fake up a function for proper names.
    ;; (unless funs
    ;;   (if (capitalizedp spell)
    ;; 	  (setf funs '(AGF::ADJ))))

    ;; Create a USAGE for each potential grammatical function of
    ;; this word.  These are just internal to the parser and not
    ;; saved until a complete parse is accepted.
    (dolist (f funs)
      (cond
	((numberp f) (incf priority f))
	((symbolp f)
	 (let (
	       (rt (make-instance 'pusage
				  :spelled spell :fn f
				  :lpos pos :rpos pos
				  :prob priority)))
	   (push rt (elt *right* pos))
	   (consider rt)))
	((null f) T))
      ;; Functions are in decreasing order of liklihood.
      (decf priority 10))))

;;; This needs to be called before each parser invocation.
(defun init-parse ()
  "Initialize the parser for a new sentence"
  (setq *right*  (make-array 15 :fill-pointer 0 :adjustable t ))
  (setq *top* NIL))

;;; Set *top* to all pairs that span the entire input string
;;; and are marked 'FINAL'.  Hopefully there is just one,
;;; and it will have all local ambiguities dealt with.
(defun select-full ()
  ;; Start with those that reach the right side.
  (let ((rt (elt *right* (minus1 (length *right*)))))
    (setq *top*
	  (remove-if
	   (lambda (x)
	     (or
	      ;; Exclude those that do NOT reach the left side
	      (> (term-lpos x) 0)
	      ;; or are not marked 'FINAL'
	      (not (has-test x 'AGF::FINAL))))
	   rt))))

;;; Remember what was said or act on commands or questions.
(defun learn (best)
  (declare (type pterm best))
  (declare (optimize (debug 3)(speed 1)))
;;  (handler-case
      (AGA:SEMANTICS best)
;;    (error (e)
;;      (progn
;;	(sb-debug:print-backtrace :count 5)
;;	(log:error e))))
  )

;;; When there is more than one possible parse, pick the one(s)
;;; with the highest 'probability'.
(defun pick-best ()
  (let* ((best-score 0)
	 (best-parse NIL))
    (dolist (candidate *top*)
      (let ((this-prob (prob candidate)))
	(cond
	  ((> this-prob best-score)
	   (progn
	     (setf best-score this-prob)
	     (setf best-parse (list candidate))))
	  ((= this-prob best-score)
	   (push candidate best-parse)))
	))
    best-parse))

;;; If there is exactly one satisfactory solution, we can
;;; learn from it or act on it.
(defun judge ()
  (declare (optimize (debug 3)(speed 1)))
  (let ((nsoln (length *top*)))
    (cond
      ((= 0 nsoln)
       (log:warn "No satisfactory solution found")
       (agg:set-parse NIL)
       NIL)

      ((= 1 nsoln)
       ;; Exactly one - we go with it.
       (learn (car *top*)))

      (T
       (log:warn "There are ~d solutions" nsoln)
       (let ((highest (pick-best)))
	 (if (= (length highest) 1)
	     (learn (car highest))
	     (progn
	       (agg:set-parse highest)
	       NIL))))
      )))

;;; Parse a list of words
(defun parse-words (wds)
  (log:info "Input ~a" wds)
  (init-parse)

  ;; Feed each word to the parser.
  (dolist (w wds) (see-word w))

  ;; Find the parses that span the entire input.
  (select-full)

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
