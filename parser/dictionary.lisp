;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGF)

;;; A dictionary of known words, keyed on spelling, taken from "pu",
;;; the cannonical reference for the toki pona language.
;;; There is one entry per spelling, but each can have multiple
;;; grammatical functions.  By convention, the grammatical function
;;; names of words are three letters long.  Longer names are for
;;; groups of words such a "prepositional phrase".  All function
;;; names are in the AGF package.
(defun wordlist ()
 '(("a" INT)("akesi" NON)("ala" ADJ NEG NUM)("alasa" VRB)
   ("ale" ADJ NON NUM)("anpa" ADJ)("ante" ADJ)("anu" POR)
   ("awen" ADJ PRV)("e" PDO)("en" AND)("esun" NON)
   ("ijo" NON)("ike" ADJ)("ilo" NON)("insa" NON)("jaki" ADJ)
   ("jan" NON)("jelo" ADJ)("jo" VRB)("kala" NON)
   ("kalama" VRB)("kama" ADJ PRV)("kasi" NON)
   ("ken" PRV ADJ)("kepeken" PRP)("kili" NON)
   ("kiwen" NON)("ko" NON)("kon" NON)("kule" ADJ)("la" CTX)
   ("lape" ADJ)("laso" ADJ)("lawa" NON VRB)("len" NON)
   ("lete" ADJ)("li" SBJ)("lili" ADJ)("linja" NON)
   ("lipu" NON)("loje" ADJ)("lon" PRP)("luka" NON NUM)
   ("lukin" NON VRB PRV)("lupa" NON)("ma" NON)("mama" NON)
   ("mani" NON)("meli" NON)("mi" P12 NON)("mike" NON)("moku" VRB NON)
   ("moli" ADJ)("monsi" NON)("mu" INT)("mun" NON)("musi" ADJ)
   ("mute" ADJ NON)("nanpa" NON NUM)("nasa" ADJ)("nasin" NON)
   ("nena" NON)("ni" ADJ)("nimi" NON)("noka" NON)("o" VOC)
   ("olin" VRB)("ona" NON)("open" VRB)("pakala" ADJ)("pali" VRB)
   ("palisa" NON)("pan" NON)("pana" VRB)("pi" POF)
   ("pilin" NON ADJ)("pimeja" ADJ)("pini" ADJ)("pipi" NON)
   ("poka" NON)("pona" ADJ)("pu" ADJ NON)("sama" ADJ PRP)
   ("seli" ADJ)("selo" NON)("seme" QUE)("sewi" ADJ NON)
   ("sijelo" NON)("sike" NON ADJ)("sin" ADJ)("sina" P12 NON)
   ("sinpin" NON)("sitelen" NON)("sona" VRB PRV)("soweli" NON)
   ("suli" ADJ)("suno" NON)("supa" NON)("suwi" ADJ)("tan" PRP)
   ("taso" BUT ADJ)("tawa" PRP ADJ)("telo" NON)("tenpo" NON)
   ("toki" VRB)("tomo" NON)("tu" NUM)("unpa" VRB)("uta" NON)
   ("utala" VRB)("walo" ADJ)("wan" ADJ NUM)("waso" NON)("wawa" ADJ)
   ("weka" ADJ)("wile" PRV)
   ;; Plus some proper names.
   ("Akato" NAM)))
