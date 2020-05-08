;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package "btree.parser")

;; A dictionary of known words, keyed on spelling.  There is one entry
;; per spelling, but it can have multiple grammatical functions.
(defvar *dict* (make-hash-table :size 20 :test 'equal))
(defun add-word (spell funs)
  (mapcar
   (lambda (x) (push x (gethash spell *dict*)))
   funs)
  )

;; Remember a word for future runs.
(defun learn-word (sp fn)
  (with-open-file (out "data/words.txt" :output)
    (format out "~a ~a~%" sp fn)
    )
  (add-word sp (list fn))
  )

;; Initialize the dictionary of known words.  Later we will read
;; this from a file.
(defun many-words (fn words)
  "Define a group of words with the same function"
  (dolist (w words) (add-word w (list fn)))
  )

(defun load-words (fname)
  "Load words from a file"
  (with-open-file (stream fname)
    (loop for strs = (agu:words-from-file stream)
       while strs do
	 (let ((spl (car strs)))
	   (if (lower-case-p (char spl 0))
	       (progn
		 (add-word spl (mapcar #'intern (cdr strs)))
		)
	   )
   ))
   ))

;;(defun filewords ()
;;  (load-words "data/words.txt")
;;  )

(defun init-words ()
  (many-words 'NN '("dog" "table" "cat" "I" "me" "you" "them" "they"))
  (many-words 'JJ '("red" "black" "fast"))
  (many-words 'IN '("under" "near" "toward" "on"))
  (many-words 'DT '("the" "a" "any" "some" "my" "your" "her" "his"))
  (many-words 'VB '("sleeps" "is" "lies" "saw"))
  (add-word "open" '(VB DJ))
  (add-word "over" '(IN DJ))
  (add-word "light" '(JJ NN))
  )

;; Print the entire dictionary.
(defun print-words ()
  (maphash
   #'(lambda (k v)
       (format T "~a -> ~a~%" k v)
       )
   *dict*))
       
;; Look up a word in the dictionary.  If not found, postulate a new
;; non-function word.
(defun lookup (spell) (gethash spell *dict*))

;; (agm:get-word spell)
