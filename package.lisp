;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(defpackage :AGADOR.SPEECH
  (:nicknames :ags)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export jstart jstop jsend say tstart *MINCONF*)
  (:export load-dictionary make-voca)
  )

(defpackage :AGADOR.MAIN
  (:nicknames :agc)
  (:use common-lisp)
  (:export run seek set-lang)
  (:export term usage pair term-fn spelled left right)
  )

;; There are a lot of grammatical function names, so they get their
;; own package namespace to avoid collisions.  Several modules
;; reference these names.
(defpackage :AGADOR.GFUN
  (:nicknames :agf)
  )

;; The utility package has string and symbol manipulation functions,
;; as well as console terminal controls.
(defpackage :AGADOR.UTIL
  (:nicknames :agu)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export words-from-file words-from-string gfun-from-string)
  (:export string-from-list clear setxy set-scroll set-color)
  (:export send connect mbx-server)
  (:export use-term release-term term clearw1 set-status)
  )

(defpackage :AGADOR.ACTION
  (:nicknames :aga)
  (:use common-lisp)
  (:export command query remember)
  )

;; Long term memory for things the parser learns, both individual words
;; and trees.  There is an underlying key-value database.
(defpackage :AGADOR.MEMORY
  (:nicknames :agm)
  (:use common-lisp)
  (:export db-open db-close db-start db-commit hash-of)
  (:export put-word get-word init-words print-words)
  (:export put-tree get-tree remember set-voice)
  (:export mterm musage mpair sig goto explore bytes-to-s)
  )

;; The actual Adjacency Parser that turns text into trees.
(defpackage :AGADOR.PARSER
  (:nicknames :agp)
  (:use common-lisp)
  (:export parse-words parse-string parse-line parse-file-line init-rules)
  (:export pterm ppair pusage start-parser parse route-path)
  (:export string-from-tree word-at)
  )
