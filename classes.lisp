;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; Here we define the most generic forms of the various objects
;;;; used to represent grammatical structure in binary tree form.
;;;; Inner nodes are called "pairs" and leaf nodes are called
;;;; "usages" or "numbers".
;;;; Subclasses of these classes are used internally to the parsing
;;;; process, and another set is used for long-term memory.
(in-package :AGC)

;;;; Pairs and Words are subclasses of Terms.  All terms have
;;;; a grammatical function taken from the AGF package.
(defclass term () (
  (fn :accessor term-fn :initarg :fn)
  (top  :accessor top :initarg :top :type integer :initform 0)
  (center :accessor center :initarg :center :type integer :initform 0)
  ))

;;;; The usage of a word represents a spelling and one possible
;;;; function.
(defclass usage (term) (
  (spelled :accessor spelled :initarg :spelled)
  ))

;;;; A pair has two subordinate terms in addition to its own grammatical
;;;; function.
(defclass pair (term) (
  (left :accessor left :initarg :left)
  (right :accessor right :initarg :right)
  ))

;; -----------------------------------------------
;;;; Walk down a btree seeking a particular grammatical function.
(defgeneric seek (term goal))

;; A usage either matches or it doesn't.
(defmethod seek ((u usage) goal) (eq (term-fn u) goal))

;; A pair might match by itself.  Otherwise we have to apply
;; knowledge of the grammar rules to find what we want.
(defmethod seek ((p pair) goal)
  (let ((start (term-fn p)))
    (if (eq start goal)
	T
	(agp:route-path start goal))))

;; -----------------------------------------------
;;;; Scan the leaves of a tree for a specific word.
(defgeneric contains-p (term goal))
(defmethod contains-p ((u usage) goal)
  (equal goal (spelled u)))
(defmethod contains-p ((p pair) goal)
  (if (contains-p (left p) goal)
      T
      (contains-p (right p) goal)))

;; -----------------------------------------------
;;;; Queries about the shape of a parse tree.
(defgeneric depth (term))
(defmethod depth ((u usage)) '(0 0))
(defmethod depth ((p pair))
  (list (1+ (max (depth (left p))))
	(1+ (max (depth (right p))))))

;;; This returns >0 if the left subtree is deeper and <0 if the
;;; right subtree is deeper.  It is used to break ties between
;;; overlapping parses of the same grammatical type when there is
;;; a preference for left- or right-association.  In toki pona, usually
;;; left-association is preferred.
(defun shape (term)
  (reduce '- (depth term)))

