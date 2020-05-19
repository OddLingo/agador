;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

(defun stop (obj)
  (let ((oname (agc:spelled obj)))
    (cond
      ((equal oname "LISTENING")
       (agm:set-voice NIL))
      (T (agu:term "Can't stop ~a~%" oname))
      )
    )
  )

(defun start (top)
    (agu:term "Acting on 'start' ~a~%" top)
  )

;; It is an instruction to do something.
(defun command (top)
  (let* ((verb (agp:word-at top 'AGF::ACTION))
	(verbname (if verb (agc:spelled verb) NIL))
	(obj (agp:word-at top 'AGF::ACTIVITY))
	)
    (cond
      ((equal verbname "STOP") (stop obj))
      ((equal verbname "START") (start obj))
      (T (agu:term "No function for ~a~%" verbname))
      )
    )
  )

;; It is some sort of statement about the world.  Just remember it.
(defun remember (top)
  ; (seek-guesses best)
  (agu:clear)
  (agm:db-start)
  (let ((key (agm:remember top)))
    (agu:term "I remember that at ~a~%" key)
    )
  (agm:db-commit)
)


;; How we answer a question depends on which query word was used.
(defun query (top)
  (let ((thing (agp:word-at top 'AGF::NOUNP)))
    (if thing
	(agu:term "You are asking about '~a'~%"
		(agp:string-from-tree thing))
	(agu:term "Seek fail~%"))
    )
  (let ((ques (agp:word-at top 'AGF::QWORD)))
    (if ques
	(agu:term "The question is '~a'~%"
		(agp:string-from-tree ques))
	(agu:term "Seek fail~%"))
    )
  )
