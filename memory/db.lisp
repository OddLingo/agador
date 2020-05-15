;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

(defvar *dbenv* NIL)
(defvar *dbtxn* NIL)
(defvar *dbw* NIL)
(defvar *dbt* NIL)
(defvar *dbc* NIL)
(defvar *dbmtx* NIL)

;;;; The overall environment, which includes the mapped data files
;;;; in which all the 'databases' are stored.
(defparameter +db-directory+
  (asdf:system-relative-pathname :agador #p"data/db/"))

;; Call open first, which creates the mapping of the files.
(defun db-open ()
  (setq *dbenv* (lmdb:make-environment +db-directory+
				       :max-databases 3
				       :mapsize (* 1 1024 1024)))
  (lmdb:open-environment *dbenv*)
  (setq *dbmtx* (sb-thread:make-mutex :name "memory mutex"))
  )

;; Call close last.  It releases the mapped file section.
(defun db-close ()
  (if *dbw* (progn (lmdb:close-database *dbw*) (setq *dbw* NIL)))
  (if *dbt* (progn (lmdb:close-database *dbt*) (setq *dbt* NIL)))
  (if *dbc* (progn (lmdb:close-database *dbc*) (setq *dbc* NIL)))
  (lmdb:close-environment *dbenv*) (setq *dbenv* NIL)
  )

;;;; A transaction must be started in order to open databases.
(defun db-start ()
  (sb-thread:grab-mutex *dbmtx*)
  (setq *dbtxn* (lmdb:make-transaction *dbenv*))
  (lmdb:begin-transaction *dbtxn*)
  (setq *dbw* (lmdb:make-database *dbtxn* "words" :create T))
  (setq *dbt* (lmdb:make-database *dbtxn* "tree" :create T))
  (setq *dbc* (lmdb:make-database *dbtxn* "context" :create T))
  (lmdb:open-database *dbw*)
  (lmdb:open-database *dbt*)
  (lmdb:open-database *dbc*)
    )

(defun db-commit ()
  (lmdb:commit-transaction *dbtxn*)
  (lmdb:close-database *dbt*) (setq *dbt* NIL)
  (lmdb:close-database *dbw*) (setq *dbw* NIL)
  (lmdb:close-database *dbc*) (setq *dbc* NIL)
  (sb-thread:release-mutex *dbmtx*)
  )

;; Convert the vector of bytes returned by LMDB into a string.
(defun to-s (data)
  (let ((bytes (coerce data '(simple-array (unsigned-byte 8) (*))))
	)
    (babel:octets-to-string bytes :encoding :utf-8)
    ))

;;;; The "words" database contains one entry per spelling.
;;;; If we know a word we return a list of all its possible
;;;; grammatical functions.  The list arrives as strings but
;;;; we 'intern all of them.
(defun get-word (k)
  (let ((data (lmdb:get *dbw* k))
	)
    (if (null data) NIL
	(mapcar
	 (lambda (x) (intern x :AGF))
	 (agu:words-from-string (to-s data))
	 )
	)
    ))

(defun put-word (spell funs)
  (lmdb:put *dbw* spell
     (format NIL "~{~a~^ ~}" funs)
  ))

;;;; The "tree" database contains one record per mterm node,
;;;; keyed by the node's Merkle signature.  Each stores a single
;;;; character string of space-separated words.

;; This the the hash function used by all tree operations.
(defun hash-of (v)
  (subseq (sha1:sha1-base64 v) 0 7))

;; We fetch a string from the database and create the coresponding
;; mterm object.  The first word in the string is a single character
;; indicating the object class.
(defun get-tree (k)
  (let ((data (lmdb:get *dbt* k))
	)
    (if data
	(let* (
	       ; Got a string.  Split into words.
	       (wds (agu:words-from-string (to-s data)))
	       ; The record type is in the first word.
	       (rty (char (car wds) 0))
	       )
	  (case rty
	    ;; Usage nodes:  (#\u BTF:FN spelling)
	    (#\u (make-instance 'musage
				:spelled (caddr wds)
				:fn (cadr wds)))
	    ;; Pair nodes: (#\p BTF:FN lefthash righthash) 
	    (#\p (make-instance 'mpair
				:fn (cadr wds)
				:left (caddr wds)
				:right (cadddr wds)))
	    ;; Anything else is an error.
	    (otherwise (progn
			 (agu:term "Bad record in DB ~a~%" data)
			 NIL))
	    )
	  )
	NIL
	)
    )
  )

;; The 'context' database stores a list of the signatures of all the
;; immediate parent nodes to the term whose signature is the key.
(defun get-context (k)
  (let ((data (lmdb:get *dbc* k))
	)
    (if (null data) NIL
	(agu:words-from-string (to-s data)))
    )
  )

;; Add a new context above a node, avoiding duplicates.
(defun add-context (child parent)
  (let* ((c (lmdb:get *dbc* child)))
    (if c
	;; Child already has contexts - check for duplicates.
	(let ((previous (agu:words-from-string (to-s c))))
	  (if (null (member parent previous))
	      (progn
		(push parent previous)
		(lmdb:put *dbc* child (agu:string-from-list previous))
		)
	      )
	  )
	;; First context for this child.
	(lmdb:put *dbc* child parent)
	)
    )
  )


;; The 'to-string' function takes care of creating the proper
;; stored representation of the data.  The key is then the
;; hash of that.
(defun put-tree (mt)
  (declare (mterm mt))
  (let* ((data (to-string mt))
	 (key (hash-of data)))
    (lmdb:put *dbt* key data)
    )
  )

(defun dump (db)
  (lmdb:do-pairs (db key data)
    (agu:term "   ~a: ~a~%"  (to-s key) (to-s data))
    ))
