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
