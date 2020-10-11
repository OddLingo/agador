;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; These are Action routines tied to certain verbs in the grammar.
;;;; Each is passed the top agp:pterm of an accepted sentence.

(in-package :AGA)

;;; Things we know about, identified by their Merkle key in the
;;; memory.
(defparameter *the-time* "26933762FB") ;; tenpo
(defparameter *the-weather* "39B8EB0A2A") ;; kon sewi
(defparameter *listening* "")  ;; kute ala
(defparameter *wakeup* "9B790DA5D3") ;; jan Akato o kute
(defparameter *sleep* "6AC8C2CF1A") ;; jan Akato o kute ala
(defvar *enabled* T)

;;; It is some sort of statement about the world.  Just remember it.
(defun remember (top)
  (declare (optimize (speed 2)(debug 3)))
  (let ((key (agm:remember top)))
    (log:info "   I remember that at ~a~%" key)
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
      ((equal handle *listening*)
       (progn
	 (log:info "Stopped listening")
	 (enable-action NIL)))
      (T (format T "Can't stop that~%")))))

(defun start (top)
    (log:info "Acting on 'start' ~a~%" top))

(defun enable-action (yes)
  (setf *enabled* yes)
  (if yes
      (ags:say "mi kute")
      (ags:say "mi kute ala")))

;;; It is an instruction to do something.
(defun command (top handle)
  "Execute explicit commands"
  (declare (type agp::pterm top)
	   (ignore top)
	   (type string handle))
  (cond
    ((equal handle *sleep*) (enable-action NIL))
    ((equal handle *wakeup*) (enable-action T))
      ;; ;; ((equal verbname "MOLI") (stop top))
      ;; ;; ((equal verbname "OPEN") (start top))
      ;; ;; ;; Next should be 'sona kama'
      ;; ;; ((equal verbname "SONA") (remember (agc:right top)))
    (T (log:warn "No action for that"))))

;;; Detect questions by the presense of the universal query
;;; word 'seme' or the VRB-NOT-VRB pattern.
(defun question-p (top)
  "Detect word 'seme' anywhere in the sentence."
  (declare (type agp::pterm top))
  (agc:contains-p top "seme"))

;;;; Questions come here.  How we answer a question depends
;;;; on which query word was used.
(defun query (top handle)
  (declare (type agp::pterm top)
	   (type string handle)
	   (ignore handle))
  (let ((answer 
	 (cond
	   ;; What is the time?
	   ;;    ((equal handle *the-time*) (saytime))
	   ;; What is the weather forecast?
	   ;;    ((equal handle *the-weather*) (wx-repeat))
	   ;; Anything else.
	   (T (let ((topw (agp:word-at top 'AGF::NON)))
		(format NIL "mi sona ala ~a"
			(if topw
			    (agp:string-from-tree topw)
			    "ona")))))))
    (log:info answer)
    (ags:say answer)))

(defun semantics (top)
  "Try to figure out the *meaning* of an utterance"
  (declare (type agp::pterm top))
  (declare (optimize (speed 2)(debug 3)))
  (let ((handle (agm::merkle top)))
    (log:info handle)
    (if *enabled*
	(cond
	  ;; Questions
	  ((question-p top)
	   (progn
	     (log:info "Question")
	     (agg:set-parse top NIL)
	     (query top handle)))

	  ;; Commands
	  ((equal (agc:term-fn top) 'AGF::CMND)
	   (progn
	     (log:info "command")
	     (agg::set-parse top NIL)
	     (command top handle)))

	  ;; Just remember anything else and give it to the explorer.
	  (T (let ((merk NIL))
	       (log:info "Remembering ~a" top)
	       (setf merk (remember top))
	       (log:info "Remembered ~a" merk)
	       (agg::set-parse top merk)
	       )
	       ))

        ;; If actions have been disabled, the only thing we listen
        ;; for is the command to start responding again.  This is
        ;; the command 'jan Akato o kute'.
	(when (equal handle *wakeup*) (enable-action T)))
  ))
