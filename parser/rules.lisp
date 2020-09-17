;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This file provides the rules for the Adjacency Parser to deal with
;;;; the toki pona language.  All rules are triplets.  The first two
;;;; terms, when found next to each other, "might" be acting as the
;;;; third term.  An optional fourth term is a function to run should
;;;; the third term end up spanning the entire input.

(in-package :AGP)
;;;; A 'routing table' of grammatical functions is derived from the
;;;; rules. It is a two-dimensional hash-table, first keyed by
;;;; the function of a 'current' node and then keyed by the function
;;;; of a 'goal' node.  The value at the intersection can be:
;;;; 1) Missing or NIL => do not proceed down this branch.
;;;; 2) T => This is the goal node
;;;; 3) AGP:LEFT => Take the left downward path
;;;; 4) AGP:RIGHT => Take the right downward path.
(defvar *route* (make-hash-table :size 50))
(defvar *rules* (make-hash-table :size 50))

(defun route-from (fn)
  (let ((ftable (gethash fn *route*)))
    (unless ftable
	(setf (gethash fn *route*) (make-hash-table :size 10)))
    (gethash fn *route*)))

(defun route-path (from toward)
  (let ((table (route-from from)))
     (gethash toward table)))

(defun add-route (startfn goal side)
  (let ((ftable (route-from startfn)))
    (unless (gethash goal ftable)
      (setf (gethash goal ftable) side))))

;;; Copy lower level routes up so we can find intermediate steps.
(defun merge-routes ()
  (loop for start being the hash-key
     using (hash-value start-table) of *route*
     do
       (loop for upper-goal being the hash-key
	  using (hash-value upper-path) of start-table
	  do
	    (let ((lower-table (gethash upper-goal *route*)))
	      (if lower-table
		  (loop for lower-goal being the hash-key of lower-table
		     do
		       (add-route start lower-goal upper-path)))))))

;;; Each entry in the rule hash-table is a list of rules with the same
;;; right term.  When trying out potential rules, we always know
;;; what the right side is, so this speeds up the search.
;;; We load the table from a big list at compile time.  The leaf terms
;;; are defined in words.lisp.
(in-package :AGF)
(defparameter +all-rules+
 '(
   (NON ADJ NON)	  ;; A modified noun phrase, left-heavy preferred
   (NON NON NON)
   (VRB ADJ VRB)   ;; Verbs too
   (PRP NON PREPP) ;; Prepositional phrase
   (NON PREPP NON) ;; Prepositions can modify nouns too
   (VRB PREPP VRB)
   (NON SBJ SSUB (NOT12))  ;; A marked Sentence subject with 'li'
   (NON NEG NON)	  ;; A negated noun  "Not green"
   (VRB NEG VRB)
   (POF NON ADJ (RIGHT))  ;; Regrouped modifier, right must be a pair.
   (NON CNJ CPFX)  ;; Left of a conjoined phrase "Apples and ..."
   (CPFX NON NON)  ;; Right of a conjoined phrase.
   (PDO NON DOBJ)  ;; A direct object
   (VRB DOBJ VRB)  ;; Only verbs can have direct objects

;; Forms of sentence.  If the word 'seme' appears, it is probably
;; a question but that gets detected at the semantic level.
;; Yes/no questions look different.
   (P12 VRB SENT (FINAL))	;; I or you do something
   (SBJ VRB PRED) ;; 'li' terminates subject
   (NON PRED SENT (FINAL))	;; Something not us does something

;; mi ijo. ; sina ijo. ; ona li ijo. ; mi mute li ijo.
;;     a! ; ...a ; noun a
;;     noun o! ; o verb... ; noun o verb
;;     noun li pre-verb verb...
;;     sentence la sentence ; fragment la sentence
;;     complex idea { sentence containing "ni": sentence

;; QUESTIONS
;;     [seme] li [seme] e [seme] prep [seme]?
;;     ...anu seme? ; noun li verb ala verb? ; yes = verb ; no = ala

;; AND, OR
;;     noun en noun ; noun li verb li verb ; noun li verb e noun e noun

;; ADJECTIVES
;;     noun + adj ; (noun + adj) adj ; ((noun + adj) adj) adj
;;     noun pi noun + adj
;;     word + Proper name {adj}

;; NUMBERS
;;     0 = ala ; 1 = wan ; 2 = tu ; 3+ = mute ; ∞ = ale
;;     alternatives; 5 = luka ; 20 = mute ; 100 = ale
;;     ordinals { noun nanpa number
))
(in-package :AGP)

;;; Create all the rule and route hash tables at compile time.
(dolist (rule AGF::+all-rules+)
  (destructuring-bind (lfn rfn rslt &optional act)
      rule
    (let ((newrule
	   (make-instance 'agp::rule
			  :left lfn :right rfn
			  :result rslt :action act))
	  (oldrules (gethash rfn AGP::*rules*)))
      ;; Add new rule to the list of all with same right term
      (setf (gethash rfn AGP::*rules*)
	    (if oldrules
		(push newrule oldrules)
		(list newrule)))
      ;; Remember paths downward through the rules.
      (agp::add-route rslt lfn 'AGC:LEFT)
      (agp::add-route rslt rfn 'AGC:RIGHT))))

;;; Find the first step of multi-step routes
(merge-routes)

;;;; A variety of tests can prevent a rule from applying.
;;;; The binary adjacency rules are rather simple.  But they
;;;; can have these more precise tests to make sure that
;;;; appplying the rules makes sense.  By returning NIL,
;;;; the test vetos the join.
(defun approve-join (act lt rt)
  "Disallow a join based on special tests"
  (labels
      ((be-pair (under)
	 (not (eq (typeof under) 'AGC::pair)))
       (notme (under)
	 (not (eq 'AGF::P12 (term-fn under)))))
    (case act
      (AGF::NOT12 (notme lt))
      (AGF::LEFT (be-pair lt))
      (AGF::RIGHT (be-pair rt))
      ((T) T))))

;;;; Here are functions that can search the routing table to
;;;; optimize the location of sub-clauses.

;;; Use the routing table to find specific parts of a sentence.
;;; Given an upper node and a desired grammatical Function,
;;; find a USAGE.
(defun word-at (start goal)
  (let ((probe start))
    (loop named searching do
	 (case (agc:seek probe goal)
	   ;; Can't get there from here
	   ((NIL) (return-from searching NIL))
	   ;; Go to left child
	   (AGC:LEFT (setf probe (agc:left probe)))
	   ;; Go to right child
	   (AGC:RIGHT (setf probe (agc:right probe)))
	   ;; This is the desired node.
	   ((T) (return-from searching probe))))))

;;; Get a list of the rules with a specified right side term.
(defun rules-for (fn) (gethash fn *rules*))

;;;; The rest are useful for debugging.
(defun print-rules ()
  "Print all rules according to right-hand term."
  (loop for rfn being the hash-key
     using (hash-value llist) of *rules*
     do
       (format T "For ~a: " rfn)
       (dolist (r llist) (print-object r T) (terpri))
       (terpri)))
       
(defun print-routes ()
  "Print out the entire route table"
  (loop for s being the hash-key
     using (hash-value st) of *route*
     do
       (agu:term "~a to~%" s)
       (loop for g being the hash-key
	  using (hash-value p) of st
	  do
	    (agu:term "    ~a go ~a~%" g p))))
