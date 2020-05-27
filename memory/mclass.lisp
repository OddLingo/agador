;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; These are the 'memory' versions of the basic Term, Usage,
;;;; and Pair relationships.  They lack the lexical attributes of
;;;; the parser versions, and the LEFT and RIGHT slots of a PAIR
;;;; could be either a 'string (in which case it is a DB key) or
;;;; a 'term reference.  The memory versions add a hashed 'signature'
;;;; which is their Merkle database key.
(defclass mterm (agc:term)
  (
   (signature
    :accessor sig
    :initarg :sig
    :type string
    :initform NIL)))

(defmethod initialize-instance :after ((mt mterm) &key)
  (unless (sig mt))
      (setf (sig mt) (merkle mt)))

(defclass mpair (agc:pair agm:mterm) ())

(defclass musage (agc:usage agm:mterm) ())

(defgeneric deref (s))
(defmethod deref ((s string)) s)
(defmethod deref ((mt mterm)) (sig mt))

;;; to-string creates the database representation of any object,
;;; either the parser or the memory form.
(defgeneric to-string (agc:term))
(defmethod to-string ((p agc:pair))
  (format NIL "p ~a ~a ~a"
	  (agc:term-fn p)
	  (deref (agc:left p))
	  (deref (agc:right p)) )
  )
(defmethod to-string ((u agc:usage))
  (format NIL "u ~a ~a" (agc:term-fn u) (agc:spelled u)) )

(defmethod merkle ((mt agc:term)) (hash-of (to-string mt)))

