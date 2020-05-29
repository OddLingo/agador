;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

;;; Things we know about, identified by their Merkle key in the
;;; memory.
(defparameter *the-time* "26933762FB")
(defparameter *the-weather* "39B8EB0A2A")
(defparameter *listening* "")

;; It is some sort of statement about the world.  Just remember it.
(defun remember (top)
  (agm:db-start)
  (let ((key (agm:remember top)))
    (agu:term "   I remember that at ~a~%" key)
    (agm:db-commit)
    key))

(defun find-again (start goal)
  (declare (optimize (debug 3)))
  (break)
  (let* ((thing (agp:word-at start goal))
	 (handle (if thing (agm:recall-p thing) NIL)))
    handle))

(defun stop (obj)
  "Stop some ongoing internal process"
  (let ((handle (find-again obj 'AGF::ACTIVITY)))
    (cond
      ((equal handle *listening*) (agm:set-voice NIL))
      (T (agu:term "")))))

(defun start (top)
    (agu:term "Acting on 'start' ~a~%" top))

;; It is an instruction to do something.
(defun command (top)
  (let* ((verb (agp:word-at top 'AGF::ACTION))
	(verbname (if verb (agc:spelled verb) NIL)))
    (cond
      ((equal verbname "STOP") (stop top))
      ((equal verbname "START") (start top))
      ((equal verbname "REMEMBER") (remember (agc:right top)))
      (T (agu:term "No function for ~a~%" verbname)))))

;; How we answer a question depends on which query word was used.
(defun query (top)
  (let ((handle (find-again top 'AGF::NOUNP)))
    (cond
      ((equal handle *the-time*) (saytime))
      ((equal handle *the-weather*) (wx-repeat))
      (T (agu:term "I do not know about '~a'~%"
	   (agp:string-from-tree (agp:word-at top 'AGF::NOUNP)))))))

