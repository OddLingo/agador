;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGS)

;; The master pronounciation dictionary is large and not used
;; very often so it is not part of the regular 'db' package.
;; We duplicate only the necessary functionality here.

(defvar *dict-env* NIL)
(defvar *dict-txn* NIL)
(defvar *dict-d* NIL)

(defconstant +dict-directory+
  (asdf:system-relative-pathname :agador #p"data/dict/"))

;; Call open first, which creates the mapping of the files.
(defun dict-open ()
  (setq *dict-env* (lmdb:make-environment
		   +dict-directory+
		   :max-databases 1
		   :mapsize (* 1 512 1024)))
  (lmdb:open-environment *dict-env*)
  )

;; Call close last.  It releases the mapped file section.
(defun dict-close ()
  (if *dict-d*
      (progn
	(lmdb:close-database *dict-d*)
	(setq *dict-d* NIL)))
  (lmdb:close-environment *dict-env*)
  (setq *dict-env* NIL)
  )

;;;; A transaction must be started in order to open databases.
(defun dict-start ()
  (setq *dict-txn* (lmdb:make-transaction *dict-env*))
  (lmdb:begin-transaction *dict-txn*)
  (setq *dict-d* (lmdb:make-database *dict-txn* "dictionary" :create T))
  (lmdb:open-database *dict-d*)
    )

(defun dict-commit ()
  (lmdb:commit-transaction *dict-txn*)
  (lmdb:close-database *dict-d*) (setq *dict-d* NIL)
  )

;;;; The "dictionary" database contains one entry per spelling.
;;;; They key is the spelling, the value is its phonetic representation.
(defun get-pronounciation (k)
  (let ((data (lmdb:get *dict-d* k))
	)
    (if (null data) NIL
	(agm:bytes-to-s data))
    )
  )

(defconstant +dict+ "^[^ ]+\\s+\\\[([^\\]]+)\\\]\\s+([a-z ]+)")
(defconstant +class+ "^% (\\w+)")
(defconstant +word+ "^([A-Z ]+)")
(defconstant +cmnt+ "^\\s*#")
(defconstant +dpath+ "~/Develop/Speech/Recognition/English/dict")
(defun load-dictionary ()
  (dict-open)
  (dict-start)
  (let ((count 0))
    (with-open-file (stream +dpath+ :direction :input)
    (loop for line = (read-line stream NIL)
       until (eq line NIL)
	 do (if line
		(ppcre:register-groups-bind
		 (spell pronounce) (+dict+ line)
		 (progn
		   (lmdb:put *dict-d* spell pronounce)
		   (setf count (1+ count))
		   )
		 ))
	 ))
    (format T "Loaded ~a words~%" count)
    )
  (dict-commit)
  (dict-close)
 )

;; Generate the Julius .voca file from a list of classes and words.
;; This relies on the dictionary to automatically look up the
;; pronounciations.
(defun make-voca (fname)
  (dict-open)
  (dict-start)
  (agm:db-start)
  (let ((in (open (format NIL "~a.words" fname)
		       :direction :input))
	(out (open (format NIL "~a.voca" fname)
			:direction :output))
	(cname NIL)
	(count 0)
	)
    (format out "% NS_B~%s	sil~%")
    (format out "% NS_E~%es	sil~%")

    (loop for line = (read-line in NIL)
	 until (eq line NIL)
       do (if line
	      (cond
		;; Ignore comments
		((ppcre:scan +cmnt+ line) T)

		;; Pick up group names
		((ppcre:register-groups-bind
		  (gname) (+class+ line)
		  (progn
		    (setf cname gname)
		    (format out "~%% ~a~%" gname))) T)

		;; Pick up words.  Tell Julius the pronounciation
		;; and the small dictionary the function.
		((ppcre:register-groups-bind
		  (spells) (+word+ line)
		  (progn
		    (let ((words (agu:words-from-string spells)))
		      (dolist (w words)
		      (let ((spell (format NIL "~a" w)))
			(format out "~a~C~a~%"
				spell #\tab (get-pronounciation spell))
			(setf count (1+ count))
			(agm:put-word spell (list cname))
			)
		      )
		    )
		  )) T)
		;;
		(T T)
		)
	      )
	 )
    (format T "Generated ~a words.  Run mkdfa again.~%" count)

    (agm:db-commit)
    (close in)
    (close out)
    )
  )

       
