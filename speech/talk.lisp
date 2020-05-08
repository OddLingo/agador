;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(require :sb-concurrency)

(in-package :ags)

;; This function sends text to a speech synthesizer.
(defun speaker (msg)
  (let ((txt (getf msg :TEXT))
	(voice (getf msg :VOICE)))
    (if (null voice) (setf voice "rms"))
    (uiop:run-program
     (format T "mimic -voice ~a -text \"~a\"" voice txt)
     :output '(:string :stripped T))
    )
  (sleep 0.6)
  )

  
(defvar *talking*)
  
;;;; External speech API

(defun tstart ()
  (setq *talking* (make-instance 'agu:mbx-server
				 :actor 'speaker))
  )

(defun say (msg)
  (agu:send *talking* '(:op :SAY :TEXT msg :voice "slt"))
  )



