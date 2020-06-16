;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGS)

;; The master pronounciation dictionary is large and not used
;; very often so it is not part of the regular 'db' package.
;; We duplicate only the necessary functionality here.

(defvar *dict-env* NIL)
(defvar *dict-txn* NIL)
(defvar *dict-d* NIL)

(defparameter +dict-directory+
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
  (setq *dict-env* NIL))

;;;; A transaction must be started in order to open databases.
(defun dict-start ()
  (setq *dict-txn* (lmdb:make-transaction *dict-env*))
  (lmdb:begin-transaction *dict-txn*)
  (setq *dict-d* (lmdb:make-database *dict-txn* "dictionary" :create T))
  (lmdb:open-database *dict-d*))

(defun dict-commit ()
  (lmdb:commit-transaction *dict-txn*)
  (lmdb:close-database *dict-d*) (setq *dict-d* NIL)
  )

;;;; The "dictionary" database contains one entry per spelling.
;;;; They key is the spelling, the value is its phonetic representation.
(defun get-pronounciation (k)
  (let ((data (lmdb:get *dict-d* k)))
    (if (null data) NIL
	(agm:bytes-to-s data))))

(defparameter +dict+ "^[^ ]+\\s+\\\[([^\\]]+)\\\]\\s+([a-z ]+)")
(defparameter +class+ "^% (\\w+)")
(defparameter +word+ "^([A-Z0-9\\.=/a-z ]+)")
(defparameter +cmnt+ "^\\s*#")
(defparameter +dpath+ "~/Develop/Speech/Recognition/English/dict")
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
		   (incf count)))))
    (format T "Loaded ~a words~%" count)))
  (dict-commit)
  (dict-close))

;;; Look for optional alternate spellings and pronounciations.
;;; This is used for numbers and names.
;;;     DICTIONARY/SPELLING
;;;     SPELLING=PHONETICS

;;; Generate the Julius .voca file from a list of classes and words.
;;; This relies on the dictionary to automatically look up the
;;; pronounciations.
(defun make-voca (fname)
  (declare (optimize (debug 3)))
  (dict-open)
  (dict-start)
  (agm:db-open)
  (agm:db-start)
  (let (;; Input file
	(in (open (format NIL "~a.words" fname)
		       :direction :input))
	;; Output file
	(out (open (format NIL "~a.voca" fname)
		   :direction :output
		   :if-exists :supersede))
	;; Sticky group name
	(cname NIL)
	;; Counter of words processed
	(count 0))

    (labels
	;; Extract spelling and phonetics in various ways.
	((modify-spelling (class inword)
	   (format T "Class ~a word ~a~%" class inword)
	   (let ((spell NIL) (phon NIL))
	     (cond
	       ;; Provide a custom spelling for numbers, etc
	       ((search "/" inword)
		(let* ((parts (cl-utilities:split-sequence '#\/ inword))
		       (dspell (first parts)))
		  (setf spell (second parts))
		  (setf phon (get-pronounciation dspell))))

	       ;; Provide a custom pronounciation for proper names, etc
	       ((search "=" inword)
		(let ((parts (cl-utilities:split-sequence '#\= inword)))
		      (setf spell (first parts))
		      (setf phon
			(ppcre:regex-replace-all
			 "[\\.]"
			 (second parts)
			 " "))))

	       ;; Just use the dicitonary pronounciation and spelling
	       (T (setf spell inword)
		  (setf phon (get-pronounciation spell))))

	     ;; Now we can add the new word to both places.
	     (format out "~a~C~a~%" spell '#\Tab phon)
	     (agm:put-word spell (list class)))))

      ;; Always need the end markers.
      (format out "% NS_B~%s	sil~%")
      (format out "% NS_E~%es	sil~%")

      ;; Now process the .words file.
      (loop for line = (read-line in NIL)
	 until (eq line NIL) do
	   (cond
	     ;; Ignore comments
	     ((ppcre:scan +cmnt+ line) T)

	     ;; Pick up group names
	     ((ppcre:register-groups-bind
	       (gname) (+class+ line)
	       (setf cname gname)
	       (format out "~%% ~a~%" gname)))

	     ;; Pick up words.  Tell Julius the pronounciation
	     ;; and the small dictionary the function.
	     ((ppcre:register-groups-bind
	       (spells) (+word+ line)
	       (let ((words (agu:words-from-string spells)))
		 (dolist (w words)
		   (modify-spelling cname w)
		   (incf count)))))
       
	     ;; Ignore everything else
	     (T T))))

    ;; Report the count.
    (format T "Generated ~a words.  Run mkdfa again.~%" count)

    (agm:db-commit)
    (agm:db-close)
    (close in)
    (close out)))

       
