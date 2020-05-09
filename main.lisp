;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

(defun run ()
  ;; Open the memory database
  (agm:db-open)

  ;; Load grammar analysis rules and start the parser thread.
  (agp:start-parser)

  ;; Start Julius speech recognizer
  (ags:jstart "data/learn")

  ;; Start the visual user interface
  (agm:explore)
  (agm:db-close)
  )


