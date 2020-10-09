;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(require :sb-concurrency)

(in-package :ags)

;; This function sends text to a speech synthesizer.
(defun speaker (msg)
  (listen-control "PAUSE")
  (sleep 0.5)

  (let ((txt (getf msg :TEXT))
	(voice (getf msg :VOICE)))
    (if (null voice) (setf voice "other/jbo"))

    ;; Stop listening while we talk.
    (uiop:run-program
     (format NIL "espeak -v ~a \"~a\"" voice txt)
     :output '(:string :stripped T))
    )
  (listen-control "RESUME")
  (sleep 3)
  )

(defvar *talking*)
  
;;;; External speech API

(defun tstart ()
  (setq *talking* (make-instance 'agu:mbx-server
				 :name "Talker"
				 :actor 'speaker))
  )

(defun say (msg &key (voice "other/jbo+m2"))
  (agu:send *talking* (list :op :SAY :TEXT msg :voice voice))
  )



