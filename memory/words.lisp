;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; A dictionary of known words, keyed on spelling.  There is one entry
;;;; per spelling, but it can have multiple grammatical functions.

(in-package :AGM)

(defun init-words ()
  "Load words from a file"
  (db-start)
  (if (null (get-word "the"))
      (with-open-file (stream "data/words.txt")
	(loop for strs = (agu:words-from-file stream)
	     while strs do
	 (let ((spl (car strs))
	       (fns (cdr strs)))
	   (format T "Adding ~a~%" spl)
	   (put-word spl (mapcar 'agu:gfun-from-string fns))
	   )
	 )))
  (db-commit)
  )

;; Print the entire active dictionary.
(defun print-words ()
  (agu:use-term)
  (agu:clear)
  (let ((x 1) (y 1))
    (lmdb:do-pairs (*dbw* spell fns)
      (agu:setxy x y)
      (format T "~a"  (bytes-to-s spell))
      (setf x (+ x 10))
      (if (> x 65)
	  (progn
	    (setf x 1)
	    (setf y (+ y 1)))
	  )
      )
    )
  (format T "~%")
  (finish-output)
  (agu:release-term)
  )
