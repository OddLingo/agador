;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

(defun word-at (start goal)
  (let ((probe start))
    (loop named searching do
       (case (agc:seek probe goal)
	 (NIL (return-from searching NIL))
	 ('AGC:LEFT (setf probe (agc:left probe)))
	 ('AGC:RIGHT (setf probe (agc:right probe)))
	 (T (return-from searching probe))
	 )
       )
    )
  )

(defun start (top)
  (agu:term "Acting on 'start' ~a~%" top)
  )

(defun stop (top)
    (agu:term "Acting on 'stop' ~a~%" top)
  )

;; It is an instruction to do something.
(defun command (top)
  (let ((verb (word-at top 'AGF::ACTION)))
    (if verb
	(format T "You want me to '~a' something.~%" (agc:spelled verb))
	(format T "Seek fail~%"))
    )
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
  (format T "Question about ~a~%" top)
  )
