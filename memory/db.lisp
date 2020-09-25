;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Low-level database operations.  The more complex 'tree'
;;;; operations are in a separate file.   The key-value "Lightning
;;;; Memory-mapped Database" LMDB is used because it is very fast.

(in-package :AGM)

;;; The overall environment, which includes the mapped data files
;;; in which all the 'databases' are stored.
(defparameter +db-directory+
  (asdf:system-relative-pathname :agador #p"data/db/"))

(defvar *dbenv* NIL)   ;; Environment handle
(defvar *dbtxn* NIL)   ;; Transaction handle
(defvar *dbmtx* NIL)   ;; Transaction Mutex

;;; When we start a transaction, we keep an a-list of the open
;;; database handles, keyed by DB ID.
(defvar *open-databases* NIL)

;;; There is also an a-list of the actual names of the databases
;;; within the environment.
(defparameter +database-names+
    '((:TREE . "tree")      ;; Remembered parse treees
      (:CNTX . "context")   ;; Upward context references
      (:TIME . "time")      ;; Decoded times
      (:INFO . "info")))    ;; Scratchpad

;;; Call open first, which creates the mapping of the files.
(defun db-open ()
  "Open the database Environment"
  (setq *dbenv* (lmdb:make-environment +db-directory+
				       :max-databases 6
				       :mapsize (* 1 1024 1024)))
  (lmdb:open-environment *dbenv*)
  (setq *dbmtx* (sb-thread:make-mutex :name "memory mutex")))

;;; Call close last.  It releases the mapped file section.
(defun db-close ()
  "Close the database Environment"
  (lmdb:close-environment *dbenv*)
  (setq *dbenv* NIL))

;;;; A transaction must be started in order to open databases.  Only
;;;; one thread at a time can open a transaction so we have a mutex too.
(defun db-start ( &optional (dbis '(:TREE :CNTX :INFO)) )
  "Start a database transaction"
  (sb-thread:grab-mutex *dbmtx*)
  (setq *dbtxn* (lmdb:make-transaction *dbenv*))
  (lmdb:begin-transaction *dbtxn*)

  ;; Open DB's identified in the dbi list and keep track of which
  ;; ones have been opened.
  (dolist (dbi dbis)
    (let* ((dbname (assoc dbi +database-names+))
	  (hdl (lmdb:make-database *dbtxn* (cdr dbname) :create T)))
      (lmdb:open-database hdl)
      (push (cons dbi hdl) *open-databases*))))

(defun db-commit ()
  "Commit changes to the database"
  ;; Commit any changes.
  (lmdb:commit-transaction *dbtxn*)
  ;; Close any DBs we opened.
  (dolist (db *open-databases*)
    (lmdb:close-database (cdr db)))
  (setq *open-databases* NIL)
  (setq *dbtxn* NIL)
  ;; Allow other threads.
  (sb-thread:release-mutex *dbmtx*))

;; Convert the vector of bytes returned by LMDB into a string.
(defun bytes-to-s (data)
  "Convert byte vectors to strings"
    (babel:octets-to-string data :encoding :utf-8))

(defun db-get (dbi key)
  "Fetch a record from a database identified by its ID"
  (let ((hdl (assoc dbi *open-databases*)))
    (unless hdl (error "Bad DB identifier ~a" dbi))
    (let ((data (lmdb:get (cdr hdl) key)))
      (if data
	  (bytes-to-s data)
	  NIL))))

(defun db-put (dbi key data)
  "Write or update a record in a database"
  (let ((hdl (assoc dbi *open-databases*)))
    (unless hdl (error "Bad DB identifier ~a" dbi))
    (lmdb:put (cdr hdl) key data)))

(defun dump (dbi)
  "Dump any database"
  (let ((hdl (assoc dbi *open-databases*)))
    (if hdl
      (lmdb:do-pairs ((cdr hdl) key data)
	(agu:term "   ~a: ~a~%"  (bytes-to-s key) (bytes-to-s data)))
      (error "Bad DB handle ~a to Dump" dbi))))

;;;; Actions can store arbitary data in the "info" database.  Since
;;;; LMDB only stores bytes, we convert the data to 'readable' format.
;;; This is usually a list of some sort.
(defun put-info (key data)
  "Write arbitrary data to the into database."
  (let* ((*print-pretty* NIL)
	 (str (write-to-string data)))
    (db-put :INFO key str)))

;;; Read and write the scratchpad.
(defun get-info (key)
  "Read arbitrary data from the 'info' database."
  (let* ((data (db-get :INFO key)))
    (if data
	(read-from-string data :eof-error-p NIL)
	NIL)))

