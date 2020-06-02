;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

;; Pairs and Words are subclasses of Terms.  All terms have
;; a grammatical function.
(defclass term () (
  (fn :accessor term-fn :initarg :fn)
  ))

;; The usage of a word represents a spelling and one possible
;; function.
(defclass usage (term) (
  (spelled :accessor spelled :initarg :spelled)
  ))

;; A pair has two subordinate terms in addition to its own grammatical
;; function.
(defclass pair (term) (
  (left :accessor left :initarg :left)
  (right :accessor right :initarg :right)
  ))

(defclass numb (term) (
  (nvalue :accessor nvalue :initarg :nvalue :initform 0)))
  
;; Walk down a btree seeking a particular function.
(defgeneric seek (term goal))

;; A usage either matches or it doesn't.
(defmethod seek ((u usage) goal) (eq (term-fn u) goal))

;; A pair might match by itslef.  Otherwise we have to apply
;; knowledge of the grammar rules to find what we want.
(defmethod seek ((p pair) goal)
  (let ((start (term-fn p)))
    (if (eq start goal)
	T
	(agp:route-path start goal)
	)
    )
  )