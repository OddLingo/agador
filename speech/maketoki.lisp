;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Generate the Julius .voca file from a list of classes and words.
;;;; As toki pono has uniform pronounciation rules, we can generate that
;;;; part on the fly.

(in-package :AGS)

(defun init-hash (h v)
  (dolist (kv v)
    (setf (gethash (car kv) h) (cdr kv))))

;;; This table maps individual letters in toki pona to their pronounciation
;;; using the Julius English Acoustic Model.  Any letter not in the table
;;; maps to itself.
(defparameter +char-pro+ (make-hash-table :test 'equal :size 20))
(init-hash
 +char-pro+
 '((#\a . "ah") (#\e . "eh") (#\i . "iy")
   (#\u . "uw") (#\o . "ow") (#\j . "zh")))
(defun pronounce (letter)
  (let ((p (gethash letter +char-pro+)))
    (if p p letter)))
(defun mapchars (s)
  (agu:string-from-list
   (map 'list #'(lambda (c) (pronounce c)) s)))

(defparameter +dict+ "^[^ ]+\\s+\\\[([^\\]]+)\\\]\\s+([a-z ]+)")
(defparameter +class+ "^% (\\w+)")
(defparameter +word+ "^([A-Z0-9\\.=/a-z ]+)")
(defparameter +cmnt+ "^\\s*#")

(defun make-voca (fname)
  (declare (optimize (debug 3)))
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
	((modify-spelling (class spell)
	   (format T "Class ~a word ~a~%" class spell)
	   (let ((phon (mapchars spell)))

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

    ;; Close all files.
    (agm:db-commit)
    (agm:db-close)
    (close in)
    (close out)))

       
