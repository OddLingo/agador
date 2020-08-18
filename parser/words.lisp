;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGP)

;;; A dictionary of known words, keyed on spelling.  There is one entry
;;; per spelling, but it can have multiple grammatical functions.  This
;;; dictionary contains *only* the function words of toki pona, though
;;; several are also nouns.
(defvar *dict* (make-hash-table :size 50 :test 'equal))
(agu:init-hash
 *dict*
 '(("a" AGF::INT) ("ala" NEG INT) ("e" DO) ("pi" OF) ("o" VOCTX INT) ("anu" OR)
 ("ike" INT) ("jaki" INT) ("mu" INT) 
 ("la" CTX) ("li" SUBJ) ("en" AND) ("lon" PREP) ("tan" PREP) ("tawa" PREP)
 ("mi" NOUN WE) ("sina" NOUN WE)
 ("jan" NCLASS) ("ante" CONTEXT) ("ken" CONTEXT PREV) ("kama" PREV)
 ("wile" PREV NOUN) ("pakala" INT) ("pona" INT) ("toki" INT NOUN)
 ("Akato" NAMES)))

;;; Print the entire dictionary.
(defun print-words ()
  (maphash
   #'(lambda (k v) (format T "~a -> ~a~%" k v))
   *dict*))
       
;;; Look up a word in the dictionary.  Anything that is not there
;;; is treated as a noun.
(defun lookup (spell)
  (let ((funs (gethash spell *dict*)))
    (if funs funs '(AGF::NOUN))))

(defun generate ()
  (let* ((funs (make-hash-table :size 30)))
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
    ;; Now write all those to the voca file, generating phonetics
    ;; along the way.
    (maphash
     #'(lambda (fn words)
	 (format T "% ~a~%~a~%" fn words))
     funs)
))
