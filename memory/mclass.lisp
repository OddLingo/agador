;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; These are the 'memory' versions of the basic Term, Usage,
;;;; and Pair relationships.  They lack the lexical attributes of
;;;; the parser versions, and the LEFT and RIGHT slots of a PAIR
;;;; could be either a 'string (in which case it is a DB key) or
;;;; a term reference.  The memory versions add a hashed 'signature'
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
(defmethod print-object ((obj mpair) stream)
  (let ((l (agc:left obj))
	(r (agc:right obj))
	(k (agm:sig obj))
	(f (agc:term-fn obj)))
    (format stream " ~a=~a/~a ~a" k f l r)))

(defclass musage (agc:usage agm:mterm) ())
(defmethod print-object ((obj musage) stream)
  (let ((s (agc:spelled obj))
	(k (agm:sig obj))
	(f (agc:term-fn obj)))
    (format stream " ~a=~a/~a" k s f)))

(defgeneric deref (s))
(defmethod deref ((s string)) s)
(defmethod deref ((pt agp::pterm)) (merkle pt))
(defmethod deref ((mt mterm)) (sig mt))

;;; string-representation creates the database representation of any
;;; tree object, in either the parser or the memory form.
(defgeneric string-representation (agc:term))
(defmethod string-representation ((p agc:pair))
  (format NIL "p ~a ~a ~a"
	  (agc:term-fn p)
	  (deref (agc:left p))
	  (deref (agc:right p)) )
  )
(defmethod string-representation ((u agc:usage))
  (format NIL "u ~a ~a" (agc:term-fn u) (agc:spelled u)) )

;;; Creates the merkle hash for generating all keys in the 'tree' and
;;; 'context' databases.  We use the high-order 40 bits of a SHA1.
(defun hash-of (v)
  (declare (type string v))
  (subseq (sha1:sha1-hex v) 0 10))

(defmethod merkle ((mt agc:term))
  (hash-of (string-representation mt)))
