;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

;;; Things we know about
(defvar *the-time* "26933762FB")

(defun stop (obj)
  (let ((otype (type-of obj)))
    (case otype
      (pusage
       (let ((oname (agc:spelled obj)))
	 (cond
	   ((equal oname "LISTENING")
	    (agm:set-voice NIL))
	   (T (agu:term "Can't stop ~a~%" oname)))))
      (ppair (agu:term "Do not know how to stop~%")))))

(defun start (top)
    (agu:term "Acting on 'start' ~a~%" top))

;; It is some sort of statement about the world.  Just remember it.
(defun remember (top)
  (agm:db-start)
  (let ((key (agm:remember top)))
    (agu:term "I remember that at ~a~%" key)
    (agm:db-commit)
    key))

;; It is an instruction to do something.
(defun command (top)
  (let* ((verb (agp:word-at top 'AGF::ACTION))
	(verbname (if verb (agc:spelled verb) NIL)))
    (cond
      ((equal verbname "STOP") (stop (agp:word-at top 'AGF::ACTIVITY)))
      ((equal verbname "START") (start (agp:word-at top 'AGF::ACTIVITY)))
      ((equal verbname "REMEMBER")
       (remember (agp:word-at top 'AGF::NOUNP)))
      (T (agu:term "No function for ~a~%" verbname)))))



;; How we answer a question depends on which query word was used.
(defun query (top)
  (let ((thing (agp:word-at top 'AGF::NOUNP)))
    (when thing
      (agu:term "You are asking about '~a'~%"
		(agp:string-from-tree thing))
      (let ((handle (remember thing)))
	(when (equal handle *the-time*)
	  (agu:term "I know what that is!~%")
	  (saytime)))))

  (let ((ques (agp:word-at top 'AGF::QWORD)))
    (if ques
	(agu:term "The question is '~a'~%"
		(agp:string-from-tree ques))
	(agu:term "Seek fail~%"))
    )
  )
