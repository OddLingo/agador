;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(defpackage :AGADOR.SPEECH
  (:nicknames :ags)
  (:use common-lisp)
  (:export jstart jsend say tstart)
  )

(defpackage :AGADOR.MAIN
  (:nicknames :btc)
  (:use common-lisp)
  (:export run)
  (:export term usage pair term-fn spelled left right)
  )

;; There are a lot of grammatical function names, so they get their
;; own package namespace to avoid collisions.  Several modules
;; reference these names.
(defpackage :BTREE.GFUN
  (:nicknames :btf)
  )

;; The utility package has string and symbol manipulation functions,
;; as well as console terminal controls.
(defpackage :AGADOR.UTIL
  (:nicknames :agu)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export words-from-file words-from-string gfun-from-string)
  (:export string-from-list clear setxy set-color)
  (:export send connect mbx-server)
  )

;; Long term memory for things the parser learns, both individual words
;; and trees.  There is an underlying key-value database.
(defpackage :AGADOR.MEMORY
  (:nicknames :agm)
  (:use common-lisp)
  (:export db-open db-close db-start db-commit hash-of)
  (:export put-word get-word init-words print-words)
  (:export put-tree get-tree remember)
  (:export mterm musage mpair sig goto explore)
  )

;; The actual Adjacency Parser that turns text into trees.
(defpackage :BTREE.PARSER
  (:nicknames :btp)
  (:use common-lisp)
  (:export parse-words parse-string parse-line parse-file-line init-rules)
  (:export pterm ppair pusage start-parser)
  )
