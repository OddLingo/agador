;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGP)

;; Within the parser, terms add a lexical range and an uncertainty.
(defclass pterm () (
  (lpos :accessor term-lpos :initarg :lpos :type integer)
  (rpos :accessor term-rpos :initarg :rpos :type integer)
  (unc :accessor term-unc :initarg :unc :type integer :initform 0)
  (seq :accessor term-seq :initarg :seq :type integer)
  ))

;; The usage of a word puts a spelling and one possible
;; function at a position in an utterance.
(defclass pusage (agc:usage agp:pterm) ())

(defmethod print-object ((obj pusage) stream)
  (let ((s (agc:spelled obj))
	(f (agc:term-fn obj))
	(p (term-rpos obj))
	(q (term-seq obj))
	(u (term-unc obj))
	)
      (if (> u 0) (format stream " [~d] ~2,'0d ~a ~a(~a)" q p s f u)
	  (format stream " [~d] ~2,'0d ~a ~a" q p s f))
      ))

;; An adjacent pair of terms have a collective function as well as
;; a span of positions.  Uncertainties are summed.
(defclass ppair (agc:pair agp:pterm)
  (
  (action :accessor action :initarg :action)
  ))

(defmethod initialize-instance :after ((obj ppair) &key)
  (setf (term-unc obj)
	(+ (term-unc (agc:left obj))
	   (term-unc (agc:right obj)))
	)
  ; Span from the lpos of the left to the rpos of the right.
  (setf (term-lpos obj) (term-lpos (agc:left obj)))
  (setf (term-rpos obj) (term-rpos (agc:right obj)))
  )

(defmethod print-object ((obj ppair) stream)
  (let ((f (agc:term-fn obj))
	(lp (term-lpos obj))
	(rp (term-rpos obj))
	(u (term-unc obj))
	(s (term-seq obj))
	)
    (if (> u 0) (format stream " [~d] ~2,'0d-~2,'0d ~a(~a)" s lp rp f u)
	  (format stream " [~d] ~2,'0d-~2,'0d ~a" s lp rp f))
	))

;; A rule describes an adjacent pair of grammatical functions that
;; should be considered together to collectively have a third function.
(defclass rule () (
  (left :accessor rule-left :initarg :left)
  (right :accessor rule-right :initarg :right)
  (result :accessor rule-result :initarg :result)
  (action :accessor action :initarg :action :initform NIL)
  ))

(defmethod print-object ((obj rule) stream)
  (let ((l (rule-left obj))
	(r (rule-right obj))
	(rs (rule-result obj))
	)
    (format stream " ~a:~a=~a" l r rs)))

;; A vector of lists of all terms with a rpos equal to the vector index.
(defvar *right*)
(defvar *top*)
(defvar *guessed*)
