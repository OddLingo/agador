;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGP)

;;; Within the parser, terms add a lexical range and an uncertainty.
(defclass pterm () (
  (lpos :accessor term-lpos :initarg :lpos :type integer)
  (rpos :accessor term-rpos :initarg :rpos :type integer)
  ))

;;; The usage of a word puts a spelling and one possible
;;; function at a position in an utterance.
(defclass pusage (agc:usage agp:pterm) ())

(defmethod print-object ((obj pusage) stream)
  (let ((s (agc:spelled obj))
	(f (agc:term-fn obj))
	(p (term-rpos obj)))
    (format stream " ~2,'0d/~a/~a" p s f)))

(defclass pnumb (agc:numb agp:pterm) ())

(defmethod print-object ((obj pnumb) stream)
  (let ((v (agc::nvalue obj)))
    (format stream " #~d" v)))
	  
;;; An adjacent pair of terms have a collective function
;;; as well as a span of positions.  They also remember
;;; any special functions that apply.
(defclass ppair (agc:pair agp:pterm)
  (			      
  (action :accessor action :initarg :action :initform NIL)))

(defmethod initialize-instance :after ((obj ppair) &key)
  ; Span from the lpos of the left to the rpos of the right.
  (setf (term-lpos obj) (term-lpos (agc:left obj)))
  (setf (term-rpos obj) (term-rpos (agc:right obj))))

(defmethod print-object ((obj ppair) stream)
  (let ((f (agc:term-fn obj))
	(lp (term-lpos obj))
	(rp (term-rpos obj))
	(a (action obj)))
    (if a
	(format stream " ~2,'0d-~2,'0d/~a:~a" lp rp f a)
	(format stream " ~2,'0d-~2,'0d/~a" lp rp f))))

;; A rule describes an adjacent pair of grammatical functions that
;; should be considered together to collectively have a third function.
(defclass rule () (
  (left :accessor rule-left :initarg :left)
  (right :accessor rule-right :initarg :right)
  (result :accessor rule-result :initarg :result)
  (action :accessor action :initarg :action :initform NIL)
  ))

(defmethod has-test ((r rule) tname)
  (not (null (member tname (action r)))))

(defmethod print-object ((obj rule) stream)
  (let ((l (rule-left obj))
	(r (rule-right obj))
	(rs (rule-result obj))
	(act (action obj)))
    (if act
	(format stream " (~a ~a ~a -> ~a)" l r rs act)
	(format stream " (~a ~a ~a)" l r rs)
	)
    )
  )

;; A vector of lists of all terms with a rpos equal to the vector index.
(defvar *right*)
(defvar *top*)
(defvar *guessed*)
