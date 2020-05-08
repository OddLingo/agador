;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; These are the 'memory' versions of the basic Term, Usage,
;;;; and Pair relationships.  They lack the lexical attributes of
;;;; the parser versions, and the LEFT and RIGHT slots of a PAIR
;;;; could be either a 'string (in which case it is a DB key) or
;;;; a 'term reference.  The memory versions add a hashed 'signature'
;;;; which is their Merkle database key.
(defclass mterm (btc:term) (
   (signature :accessor sig :initarg :sig :initform NIL)))
(defmethod initialize-instance :after ((mt mterm) &key)
  (if (null (sig mt))
      (setf (sig mt) (merkle mt))))

(defclass mpair (btc:pair agm:mterm) ())

(defclass musage (btc:usage agm:mterm) ())

(defgeneric deref (s))
(defmethod deref ((s string)) s)
(defmethod deref ((mt mterm)) (sig mt))

;; to-string creates the database representation of an mterm object.
(defgeneric to-string (mterm))
(defmethod to-string ((p mpair))
  (format NIL "p ~a ~a ~a"
	  (btc:term-fn p)
	  (deref (btc:left p))
	  (deref (btc:right p)) )
  )
(defmethod to-string ((u musage))
  (format NIL "u ~a ~a" (btc:term-fn u) (btc:spelled u)) )

(defmethod merkle ((mt mterm)) (hash-of (to-string mt)))

