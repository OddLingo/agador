;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

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
  )

;; There are a lot of grammatical function names, so they get their
;; own package namespace to avoid collisions.  Several modules
;; reference these names.
(defpackage :AGADOR.GFUN
  (:nicknames :agf)
  (:use common-lisp)
  )

;; The utility package has string and symbol manipulation functions,
;; as well as console terminal controls.
(defpackage :AGADOR.UTIL
  (:nicknames :agu)
  (:use common-lisp)
  (:import-from :cl-utilities :split-sequence)
  (:export words-from-file words-from-string gfun-from-string)
  (:export string-from-list clear setxy set-scroll set-color clear-eol)
  (:export send connect mbx-server)
  (:export use-term release-term term clearw1 set-status)
  (:export sked-add sked-later init-hash)
  )

(defpackage :AGADOR.ACTION
  (:nicknames :aga)
  (:use common-lisp)
  (:export semantics remember)
  (:export wx-tropical)
  (:export start-security speakable-time)
  )

;; Long term memory for things the parser learns, both individual words
;; and trees.  There is an underlying key-value database.
(defpackage :AGADOR.MEMORY
  (:nicknames :agm)
  (:use common-lisp)
  (:export db-open db-close db-start db-commit hash-of)
  (:export get-info put-info)
  (:export put-tree get-tree remember recall-p set-voice db-put db-get)
  (:export mterm musage mpair sig goto explore bytes-to-s merkle)
  )

;; The actual Adjacency Parser that turns text into trees.
(defpackage :AGADOR.PARSER
  (:nicknames :agp)
  (:use common-lisp)
  (:export parse-words parse-string parse-line parse-file-line init-rules)
  (:export pterm ppair pusage pnumb start-parser parse route-path)
  (:export string-from-tree word-at term-lpos term-rpos print-words)
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

