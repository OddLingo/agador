;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This file provides dictionary services to the parser.
(in-package :AGP)

;;; A dictionary of all the words in toki pona, keyed on spelling,
;;; taken from "pu", the canonical reference for the toki pona language.
;;; There is one entry per spelling, but each can have multiple
;;; grammatical functions.  By convention, the grammatical function
;;; names of words are three letters long.  Longer names are for
;;; groups of words such a "prepositional phrase".  All function
;;; names are in the AGF package.

;;; Signed numbers can be placed in the list of functions in order
;;; to modify the priority of following terms.  Normally the first
;;; term has a priority of 100 and each sucessive term is 10 less.
(defvar *dict* (make-hash-table :size 140 :test 'equal))
(in-package :AGF)
(dolist (word
 '(("a" INT)("akesi" NON)("ala" ADJ NEG NUM)("alasa" VRB)
   ("ale" ADJ NON NUM)("ali" ADJ NON NUM)("anpa" ADJ)("ante" ADJ)
   ("anu" CNJ)("awen" ADJ PRV VRB)
   ("e" PDO)("en" CNJ)("esun" NON)
   ("ijo" NON)("ike" ADJ)("ilo" NON)("insa" NON)("jaki" ADJ)
   ("jan" NON)("jelo" ADJ)("jo" VRB)("kala" NON)
   ("kalama" VRB)("kama" PRV ADJ)("kasi" NON)("ken" PRV ADJ)
   ("kepeken" PRP)("kili" NON)("kiwen" NON)("ko" NON)
   ("kon" NON)("kule" ADJ)("kute" VRB)("la" CTX)
   ("lape" ADJ)("laso" ADJ)("lawa" NON VRB)("len" NON)
   ("lete" ADJ)("li" SBJ)("lili" ADJ)("linja" NON)
   ("lipu" NON)("loje" ADJ)("lon" PRP)("luka" NON NUM)
   ("lukin" VRB PRV NON)("lupa" NON)("ma" NON PNA)("mama" NON)
   ("mani" NON)("meli" NON)("mi" P12 ADJ)("mike" NON)
   ("moku" VRB -10 NON)
   ("moli" ADJ)("monsi" NON)("mu" INT)("mun" NON)("musi" ADJ)
   ("mije" NON)
   ("mute" ADJ NON)("nanpa" NON NUM)("nasa" ADJ)("nasin" NON)
   ("nena" NON)("ni" ADJ)("nimi" NON)("noka" NON)
   ("o" VOC)("olin" VRB)("ona" NON ADJ)("open" VRB)
   ("pakala" VRB ADJ)("pali" VRB)
   ("palisa" NON)("pan" NON)("pana" VRB)("pi" POF)
   ("pilin" NON ADJ)("pimeja" ADJ)("pini" ADJ)("pipi" NON)
   ("poka" NON)("pona" ADJ)("pu" ADJ NON)
   ("sama" PRP ADH)
   ("seli" ADJ)("selo" NON)("seme" NON)("sewi" ADJ NON)
   ("sijelo" NON)("sike" NON ADJ)("sin" ADJ)("sina" P12 ADJ)
   ("sinpin" NON)("sitelen" NON)("sona" VRB PRV)("soweli" NON)
   ("suli" ADJ)("suno" NON)("supa" NON)("suwi" ADJ)
   ("tan" PRP)
   ("taso" BUT ADJ)("tawa" PRP ADJ)("telo" NON)("tenpo" NON)
   ("toki" VRB NON)("tomo" NON PNA)("tu" NUM)
   ("unpa" VRB)("uta" NON)("utala" VRB)
   ("walo" ADJ)("wan" ADJ NUM)("waso" NON)("wawa" ADJ)
   ("weka" ADJ)("wile" PRV)
   ;; Plus some proper names, which act like adjectives.
   ("Akato" ADJ)("Mewika" ADJ)))
  (setf (gethash (car word) AGP::*dict*) (cdr word)))

(in-package :AGP)

;;; This table maps individual letters in toki pona to their pronounciation
;;; in the Acoustic Model.  Any letter not in the table
;;; maps to itself.  Change to match acoustic model.
(defparameter +char-sound+ (make-hash-table :test 'equal :size 10))
(agu:init-hash
 +char-sound+
 '(("a" . "ah") ("e" . "eh") ("i" . "iy")
   ("u" . "uw") ("o" . "ow") ("j" . "y")))

;;; We need to tell the Recognizer how each word is pronounced.
;;; This function provides that information.
(defun phonetics (s)
  "Convert a TP word to its pronounciation"
  (labels
      ((letter-sound (letter)
	 (let ((lower (string-downcase letter)))
	   (gethash lower +char-sound+ lower))))

    (agu:string-from-list
     (map 'list #'(lambda (c) (letter-sound c)) s))))

;;; Look up a word in the dictionary.
(defun lookup (spell)
  (gethash spell *dict*))

(defun all-words (callback)
  (dolist (w (alexandria:hash-table-keys *dict*))
    (funcall callback w (phonetics w))))

;;; Create an inverted hash of the dictionary, so all words with
;;; the same function are grouped together.  This is used by the
;;; corpus-generator.
(defun words-by-fn ()
  (let* ((funs (make-hash-table :size 30)))
    (maphash
     #'(lambda (spell word-funs)
	 (dolist (fn word-funs)
	   ;; Skip over NILs and numbers.
	   (when (symbolp fn)
	     (let ((old (gethash fn funs)))
	       (setf
		(gethash fn funs)
		(if old
		    (push spell old)
		    (list spell))))
	     )))
     AGP::*dict*)
    funs))

