;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; A database containing a Merkle tree of all parsed sentences
;;;; where all common subtrees are merged.
;;;; A record key is the 20-byte SHA1 hash of the contents.

(in-package :AGM)

;;; Clone a parser tree object into long-term memory.  Note that
;;; pairs are cloned recursively.  The value returned is always
;;; Merkle key of the remembered object.  
(defgeneric remember (pterm))
(defmethod remember ((u btp:pusage))
  (let ((m (make-instance 'musage
	:fn (btc:term-fn u)
	:spelled (btc:spelled u))))
    (if (null (get-tree (sig m)))
	(progn
	  (put-tree m)
	  ))
    (sig m)
    ))
(defmethod remember ((p btp:ppair))
  (let* ((lc (remember (btc:left p)))
	 (rc (remember (btc:right p)))
	 (m (make-instance 'mpair
			   :fn (btc:term-fn p)
			   :left lc
			   :right rc))
	 (msig (sig m))
	 )
    (if (null (get-tree (sig m)))
	(progn
	  (put-tree m)
	  ; do not save contexts for stopwords (a, the, and, etc)
	  (add-context lc msig)
	  (add-context rc msig)
	  )
	)
    (sig m)
    ))
(defmethod remember ((s string)) s)
