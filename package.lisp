;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;; The speech package manages interfaces to both recognition
;; and synthesis of audible speech.
(defpackage :AGADOR.SPEECH
  (:nicknames :ags)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export listen-start listen-stop listen-control say tstart *MINCONF*)
  (:export generate)
  )

(defpackage :AGADOR.MAIN
  (:nicknames :agc)
  (:use common-lisp)
  (:export run seek set-lang +data-directory+)
  (:export term usage pair numb term-fn spelled left right nvalue)
  (:export contains-p depth shape)
  (:export center top)
  )

;; There are a lot of grammatical function names, so they get their
;; own package namespace to avoid collisions.  Several modules
;; reference these names.
(defpackage :AGADOR.GFUN
  (:nicknames :agf)
  (:use common-lisp)
  )

;; The utility package has string and symbol manipulation functions.
(defpackage :AGADOR.UTIL
  (:nicknames :agu)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export words-from-file words-from-string gfun-from-string)
  (:export string-from-list)
  (:export send connect mbx-server)
  (:export sked-add sked-later init-hash)
  )

;; The graphical user interface, based on McCLIM.
(defpackage :AGADOR.GUI
  (:nicknames :AGG)
  (:use :clim :clim-lisp)
  (:export run-window *app* set-input set-parse)
  (:export set-status set-output))

;; The action package actually does things based on
;; input sentences.
(defpackage :AGADOR.ACTION
  (:nicknames :aga)
  (:use common-lisp)
  (:export semantics remember)
  (:export wx-tropical)
  (:export start-security speakable-time)
  )

;; Long term memory for things the parser learns.
;; There is an underlying key-value database.
(defpackage :AGADOR.MEMORY
  (:nicknames :agm)
  (:use common-lisp)
  (:export db-open db-close db-start db-commit hash-of)
  (:export get-info put-info get-context with-memory)
  (:export string-from-tree put-tree get-tree)
  (:export remember recall-p set-voice db-put db-get)
  (:export mterm musage mpair sig bytes-to-s merkle)
  )

;; The actual Adjacency Parser that turns text into trees.
(defpackage :AGADOR.PARSER
  (:nicknames :agp)
  (:use common-lisp)
  (:export parse-words parse-string parse-line)
  (:export parse-file-line init-rules)
  (:export pterm ppair pusage start-parser parse route-path)
  (:export string-from-tree word-at term-lpos term-rpos)
  )

(defpackage :AGADOR.SHA1
  (:nicknames :sha1)
  (:use :cl)
  (:export
   #:sha1-digest
   #:sha1-hex

   ;; HMAC functions
   #:hmac-sha1-digest
   #:hmac-sha1-hex
   ))

