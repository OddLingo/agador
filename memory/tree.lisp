;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; A database containing a Merkle tree of all parsed sentences
;;;; where all common subtrees are merged.
;;;; A record key is the 20-byte SHA1 hash of the contents.

(in-package :AGM)

;;; Clone a parser tree object into long-term memory.  Note that
;;; pairs are cloned recursively.  The value returned is always the
;;; Merkle key of the remembered object.  
(defgeneric remember (pterm))
(defmethod remember ((u agp:pusage))
  (let ((m (make-instance 'musage
	:fn (agc:term-fn u)
	:spelled (agc:spelled u))))
    ; If it does not already exist, create it in the db.
    (if (null (get-tree (sig m))) (put-tree m))	  
    (sig m)
    ))
(defmethod remember ((p agp:ppair))
  (let* ((lc (remember (agc:left p)))
	 (rc (remember (agc:right p)))
	 (m (make-instance 'mpair
			   :fn (agc:term-fn p)
			   :left lc
			   :right rc))
	 (msig (sig m))
	 )
    ; If this exact pair is not already in the db, create it
    ; and the contexts up form the lower nodes.
    (if (null (get-tree msig))
	(progn
	  (put-tree m)
	  ; do not save contexts for stopwords (a, the, and, etc)
	  (add-context lc msig)
	  (add-context rc msig)
	  )
	)
    msig
    ))
(defmethod remember ((s string)) s)
