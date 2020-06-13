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
  (let ((uni-time (time-check top))
	(key (agm:remember top)))
    ;; If the statement contains a time reference, keep track of that.
    (when uni-time
      (AGM:DB-PUT :TIME key (format NIL "~d" uni-time)))
    (agu:term "   I remember that at ~a~%" key)
    (agm:db-commit)
    key))

;;; 'merkle-of-subtree' tries to discover the database key for the memory
;;; representation of some sub-tree of a given parse tree.
(defun merkle-of-subtree (start goal)
  (declare (type agp::pterm start))
  (let ((subtree (agp:word-at start goal)))
    (if subtree (agm:merkle subtree) NIL)))

(defun stop (obj)
  "Stop some ongoing internal process"
  (let ((handle (merkle-of-subtree obj 'AGF::ACTIVITY)))
    (cond
      ((equal handle *listening*) (agm:set-voice NIL))
      (T (agu:term "")))))

(defun start (top)
    (agu:term "Acting on 'start' ~a~%" top))

;; It is an instruction to do something.
(defun command (top)
  (declare (type agp::pterm top))
  (let* ((verb (agp:word-at top 'AGF::ACTION))
	(verbname (if verb (agc:spelled verb) NIL)))
    (cond
      ((equal verbname "STOP") (stop top))
      ((equal verbname "START") (start top))
      ((equal verbname "REMEMBER") (remember (agc:right top)))
      (T (agu:term "No function for ~a~%" verbname)))))

;; How we answer a question depends on which query word was used.
(defun query (top)
  (declare (type agp::pterm top))
  (let ((handle (merkle-of-subtree top 'AGF::NOUNP)))
    (cond
      ;; What is the time?
      ((equal handle *the-time*) (saytime))
      ;; What is the weather forecast?
      ((equal handle *the-weather*) (wx-repeat))
      ;; Anyting else.
      (T (agu:term "I do not know about '~a'~%"
	   (agp:string-from-tree (agp:word-at top 'AGF::NOUNP)))))))

