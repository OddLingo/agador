;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This file provides dictionary services to the parser.
(in-package :AGP)

;;; A dictionary of known words, keyed on spelling.  There is one entry
;;; per spelling, but it can have multiple grammatical functions.  This
;;; dictionary contains *only* the function words of toki pona, though
;;; several are also nouns.
(defvar *dict* (make-hash-table :size 40 :test 'equal))
(agu:init-hash
 *dict*
 '(("a" INT) ("ala" NEG INT NOUN) ("e" DO) ("pi" OF)
   ("o" VOCTX INT) ("anu" OR) ("ike" INT NOUN) ("jaki" INT NOUN)
   ("mu" INT) ("la" CTX) ("li" SUBJ) ("en" AND) ("lon" PREP NOUN)
   ("tan" PREP) ("tawa" PREP) ("mi" NOUN WE) ("sina" NOUN WE)
   ("jan" NCLASS) ("ante" CONTEXT NOUN) ("ken" CONTEXT NOUN PREV)
   ("kama" PREV) ("wile" PREV NOUN) ("pakala" INT) ("pona" INT)
 ("toki" INT NOUN) ("Akato" NAMES)))

;;; Just the noun, adjective, verb words.  These go into the VOCA file
;;; for Julius, but we don't need them here.
(defparameter +nouns+
  '("akesi" "ala" "ale" "ali" "anpa" "ante" "awen" "ijo" "ike"
    "ilo" "insa" "jaki" "jan" "jelo" "jo" "kala" "kalama" "kama" "kasi"
    "ken" "kepeken" "kili" "kin" "kiwen" "kule" "kulupu" "kute" "lape"
    "laso" "lawa" "len" "lete" "lili" "linja" "lipu" "loje" "lon" "luka"
    "lukin" "lupa" "ma" "mama" "mani" "meli" "mi" "mije" "moku" "moli"
    "monsi" "mun" "musi" "mute" "nanpa" "nasa" "nasin" "nena" "ni" "nimi"
    "noka" "nouns" "oko" "olin" "ona" "open" "pakala" "pali" "palisa"
    "pana" "pilin" "pimeja" "pini" "pipi" "poka" "poki" "pona" "sama"
    "seli" "selo" "seme" "sewi" "sijelo" "sike" "sima" "sin" "sina"
    "sinpin" "sitelen" "sona" "soweli" "suli" "suno" "supa" "suwi" "tan"
    "taso" "tawa" "telo" "tenpo" "the" "toki" "tomo" "tu" "unpa" "uta"
    "utala" "walo" "wan" "waso" "wawa" "weka" "wike" "wile"))

;;; This table maps individual letters in toki pona to their pronounciation
;;; using the Julius English Acoustic Model.  Any letter not in the table
;;; maps to itself.
(defparameter +char-sound+ (make-hash-table :test 'equal :size 20))
(agu:init-hash
 +char-sound+
 '(("a" . "ah") ("e" . "eh") ("i" . "iy")
   ("u" . "uw") ("o" . "ow") ("j" . "zh")))

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
       
;;; Look up a word in the dictionary.  Anything that is not there
;;; is reported as a noun.
(defun lookup (spell)
  (gethash spell *dict* '(AGP::NOUN)))

;;;; The entire language dictionary is in this file, above.  But it needs
;;;; to be in a special file for the Julius speech recognizer, along
;;;; with phonetic information.  Here is where we generate that file.
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

      ;; Now write all those except NOUN to the voca file,
      ;; generating phonetics along the way.
      (dolist (fn (alexandria:hash-table-keys funs))
	(unless (eq fn 'NOUN)
	  (fn-to-voca fn)))

      ;; The NOUN case is handled separately.
      (format out "~%% NOUN~%")
      (dolist (word +nouns+)
	(print-word word))
      )
    (close out)
))
