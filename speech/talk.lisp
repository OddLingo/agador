;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(require :sb-concurrency)

(in-package :ags)

;; This function sends text to a speech synthesizer.
(defun speaker (msg)
  (let ((txt (getf msg :TEXT))
	(voice (getf msg :VOICE)))
    (if (null voice) (setf voice "rms"))

    ;; Tell Julius to stop listening while we talk.
;    (jsend "PAUSE\n")
    (sleep 0.5)
    (uiop:run-program
     (format NIL "mimic -voice ~a \" ~a\"" voice txt)
     :output '(:string :stripped T))
    )
;  (jsend "RESUME\n")
  (sleep 0.6)
  )

  
(defvar *talking*)
  
;;;; External speech API

(defun tstart ()
  (setq *talking* (make-instance 'agu:mbx-server
				 :name "Talker"
				 :actor 'speaker))
  )

(defun say (msg)
  (agu:send *talking* (list :op :SAY :TEXT msg :voice "slt"))
  )



