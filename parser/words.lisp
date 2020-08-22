;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This file provides dictionary services to the parser.
(in-package :AGP)

(defvar *dict* (make-hash-table :size 140 :test 'equal))
(agu:init-hash *dict* (AGF:wordlist))

;;; This table maps individual letters in toki pona to their pronounciation
;;; using the Julius English Acoustic Model.  Any letter not in the table
;;; maps to itself.
(defparameter +char-sound+ (make-hash-table :test 'equal :size 20))
(agu:init-hash
 +char-sound+
 '(("a" . "ah") ("e" . "eh") ("i" . "iy")
   ("u" . "uw") ("o" . "ow") ("j" . "y")))

(defun phonetics (s)
  "Convert a TP word to its pronounciation"
  (labels
      ((letter-sound (letter)
	 (let ((lower (string-downcase letter)))
	   (gethash lower +char-sound+ lower))))

    (agu:string-from-list
     (map 'list #'(lambda (c) (letter-sound c)) s))))

;;; Print the entire internal dictionary.
(defun print-words ()
  (maphash
   #'(lambda (k v) (format T "~a -> ~a~%" k v))
   *dict*))
       
;;; Look up a word in the dictionary.
(defun lookup (spell)
  (gethash spell *dict*))

;;;; The dictionary needs to be in a special format for the Julius
;;;; speech recognizer, along with phonetic information.  Here
;;;; is where we generate that file.
(defun generate ()
  (let* ((funs (make-hash-table :size 30))
	 (out (open
	       (format NIL "~a/toki.voca" AGC:+data-directory+)
	       :direction :output
	       :if-exists :supersede)))

    ;; First invert the dictionary so all words with the same function
    ;; are grouped together.
    (maphash
     #'(lambda (spell word-funs)
	 (dolist (fn word-funs)
	   (let ((old (gethash fn funs)))
	     (setf
	      (gethash fn funs)
	      (if old
		  (push spell old)
		  (list spell))))
	   ))
     *dict*)

    (labels
	((print-word (word)
	   (format out "~a~C~a~%" word '#\Tab (phonetics word)))
	 (fn-to-voca (fn)
	   (format out "~%% ~a~%" fn)
	   (dolist (word (gethash fn funs))
	     (print-word word))))

      ;; The 'silence markers' are always there.
      (format out "# This is a generated file.  Do not edit.~%")
      (format out "% NS_B~%s	sil~%")
      (format out "% NS_E~%es	sil~%")

      ;; Now write all the words to the voca file, grouped by function,
      ;; generating phonetics along the way.
      (dolist (fn (alexandria:hash-table-keys funs))
	  (fn-to-voca fn)))
    (close out)
))
