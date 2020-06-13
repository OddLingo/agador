;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; The "tree" database contains one record per term,
;;;; keyed by the node's Merkle signature.  Each stores a single
;;;; character string of space-separated words.

;;; We fetch a string from the database and create the coresponding
;;; mterm object.  The first word in the string is a single character
;;; indicating the object class.  Numbers are a special case where
;;; the key starts with the letter "N" and the rest is a decimal number.
(defun get-tree (key)
  "Create mterm object from the database."
  (declare (type string key))
  (when (equal (char key 0) '#\N)
    (return-from get-tree (parse-integer key :start 1)))

  (let ((data (db-get :TREE key)))
    (if data
	(let* (
	       ; Got a string.  Split into words.
	       (wds (agu:words-from-string data))
	       ; The record type is in the first word.
	       (rty (char (car wds) 0))
	       )
	  (case rty
	    ;; Usage nodes:  (#\u AGF:FN spelling)
	    (#\u (make-instance 'musage
				:spelled (caddr wds)
				:fn (intern (cadr wds) :AGF)))
	    ;; Pair nodes: (#\p AGF:FN lefthash righthash) 
	    (#\p (make-instance 'mpair
				:fn (intern (cadr wds) :AGF)
				:left (caddr wds)
				:right (cadddr wds)))
	    ;; Anything else is an error.
	    (otherwise
	     (agu:term "Bad record in DB ~a~%" data)
	     NIL))))
	NIL))

;;; The 'context' database stores a list of the signatures of all the
;;; immediate parent nodes to the term whose signature is the key.
(defun get-context (key)
  (let ((data (db-get :CNTX key)))
    (if data
	(agu:words-from-string data)
	NIL)))

;;; Add a new context above a node, avoiding duplicates.
(defun add-context (child parent)
  (let* ((c (get :CNTX child)))
    (if c
	;; Child already has contexts - check for duplicates.
	(let ((previous (agu:words-from-string (bytes-to-s c))))
	  (unless (member parent previous)
		(push parent previous)
		(db-put :CNTX child (agu:string-from-list previous))))
	;; First context for this child.
	(db-put :CNTX child parent)
	)))

;;; The 'to-string' function takes care of creating the proper
;;; stored representation of the data.  The key is then the
;;; hash of that.
(defun put-tree (mt)
  (declare (mterm mt))
  (let* ((data (string-representation mt))
	 (key (hash-of data)))
    (db-put :TREE key data)))

;;; Clone a parser tree structure into long-term memory.  Note that
;;; pairs are cloned recursively.  The value returned is always the
;;; Merkle key of the remembered object.  
(defgeneric remember (pterm))
(defmethod remember ((n agp:pnumb))
  (format NIL "N~d" (agc:nvalue n)))

(defmethod remember ((u agp:pusage))
  (let ((m (make-instance 'musage
	:fn (agc:term-fn u)
	:spelled (agc:spelled u))))
    ; If it does not already exist, create it in the db.
    (unless (get-tree (sig m)) (put-tree m))	  
    (sig m)
    ))

(defmethod remember ((p agp:ppair))
  (let* ((lc (remember (agc:left p)))
	 (rc (remember (agc:right p)))
	 (m (make-instance 'mpair
			   :fn (agc:term-fn p)
			   :left lc
			   :right rc))
	 (msig (sig m)))

    ;; If this exact pair is not already in the db, create it
    ;; and the contexts up from the lower nodes.
    (unless (get-tree msig)
	(progn
	  (put-tree m)
	  ; do not save contexts for stopwords (a, the, and, etc)
	  (add-context lc msig)
	  (add-context rc msig)))
    msig))
(defmethod remember ((s string)) s)

;;;; Recalling is the inverse of remembering.  We use the same
;;;; recursive descent and ascent, but do not create anything
;;;; that is missing.
(defgeneric recall-p (agp:pterm))
(defmethod recall-p ((node agp:pusage))
  (get-tree (merkle node)))
(defmethod recall-p ((num agp:pnumb)) T)
(defmethod recall-p ((p agp:ppair))
  (unless (recall-p (agc:left p)) (return-from recall-p NIL))
  (unless (recall-p (agc:right p)) (return-from recall-p NIL))
  (get-tree (merkle p)))

(defmethod recall-p ((s string)) s)
