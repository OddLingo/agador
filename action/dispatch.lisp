;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

(defun start (top)
  (agu:term "Acting on 'start' ~a~%" top)
  )

(defun stop (top)
    (agu:term "Acting on 'stop' ~a~%" top)
  )

(defun command (top)
  (agu:term "Acting on command ~a~%" top)
  )

;; It is some sort of statement about the world.  Just remember it.
(defun remember (top)
  ; (seek-guesses best)
  (agu:clear)
  (agm:db-start)
  (agm:remember top)
  (agm:db-commit)
)

;; How we answer a question depends on which query word was used.
(defun query (top)
  (let ((q (seek top AGF::QWORD)))
    )

  )
