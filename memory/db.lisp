;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; Low-level database operations.  The more complex 'tree'
;;;; operations are in a separate file.   The key-value "Lightning
;;;; Memory-mapped Database" LMDB is used because it is very fast.

(defvar *dbenv* NIL)
(defvar *dbtxn* NIL)
(defvar *dbw* NIL)
(defvar *dbt* NIL)
(defvar *dbc* NIL)
(defvar *dbi* NIL)
(defvar *dbmtx* NIL)

;;;; The overall environment, which includes the mapped data files
;;;; in which all the 'databases' are stored.
(defparameter +db-directory+
  (asdf:system-relative-pathname :agador #p"data/db/"))

;; Call open first, which creates the mapping of the files.
(defun db-open ()
  "Open the database Environment"
  (setq *dbenv* (lmdb:make-environment +db-directory+
				       :max-databases 4
				       :mapsize (* 1 1024 1024)))
  (lmdb:open-environment *dbenv*)
  (setq *dbmtx* (sb-thread:make-mutex :name "memory mutex")))

;; Call close last.  It releases the mapped file section.
(defun db-close ()
  "Close the database Environment"
  (when *dbw* (lmdb:close-database *dbw*) (setq *dbw* NIL))
  (when *dbt* (lmdb:close-database *dbt*) (setq *dbt* NIL))
  (when *dbc* (lmdb:close-database *dbc*) (setq *dbc* NIL))
  (when *dbi* (lmdb:close-database *dbc*) (setq *dbi* NIL))
  (lmdb:close-environment *dbenv*) (setq *dbenv* NIL)
  )

;;;; A transaction must be started in order to open databases.
(defun db-start ()
  "Start a database transaction"
  (sb-thread:grab-mutex *dbmtx*)
  (setq *dbtxn* (lmdb:make-transaction *dbenv*))
  (lmdb:begin-transaction *dbtxn*)
  (setq *dbw* (lmdb:make-database *dbtxn* "words" :create T))
  (setq *dbt* (lmdb:make-database *dbtxn* "tree" :create T))
  (setq *dbc* (lmdb:make-database *dbtxn* "context" :create T))
  (setq *dbi* (lmdb:make-database *dbtxn* "info" :create T))
  (lmdb:open-database *dbw*)
  (lmdb:open-database *dbt*)
  (lmdb:open-database *dbc*)
  (lmdb:open-database *dbi*))

(defun db-commit ()
  "Commit changes to the database"
  (lmdb:commit-transaction *dbtxn*)
  (lmdb:close-database *dbt*) (setq *dbt* NIL)
  (lmdb:close-database *dbw*) (setq *dbw* NIL)
  (lmdb:close-database *dbc*) (setq *dbc* NIL)
  (lmdb:close-database *dbi*) (setq *dbi* NIL)
  (sb-thread:release-mutex *dbmtx*)
  )

;; Convert the vector of bytes returned by LMDB into a string.
(defun bytes-to-s (data)
  "Convert byte vectors to strings"
  (let ((bytes (coerce data '(simple-array (unsigned-byte 8) (*))))
	)
    (babel:octets-to-string bytes :encoding :utf-8)
    ))

;;;; The "words" database contains one entry per spelling.
;;;; If we know a word we return a list of all its possible
;;;; grammatical functions.  The list arrives as strings but
;;;; we 'intern all of them.
(defun get-word (k)
  (let ((data (lmdb:get *dbw* k)))
    (if data
	(dolist (x (agu:words-from-string (bytes-to-s data)))
	  (intern x :AGF))
	NIL)))

(defun put-word (spell funs)
  (lmdb:put *dbw* spell
     (format NIL "~{~a~^ ~}" funs)))

(defun dump (db)
  (lmdb:do-pairs (db key data)
    (agu:term "   ~a: ~a~%"  (bytes-to-s key) (bytes-to-s data))
    ))

;;; Read and write the scratchpad.
(defun get-info (key)
  (ignore-errors
    (read-from-string
     (bytes-to-s (lmdb:get *dbi* key)) :eof-error-p NIL)))

(defun put-info (key data)
  (lmdb:put *dbi* key (format NIL "~S" data)))
