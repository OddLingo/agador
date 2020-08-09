;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; Maintain a log file with timestamps.
(in-package :AGU)

;;; A mutex prevents threads from mixing output lines.
(defvar *logmtx* (sb-thread:make-mutex :name "Logging"))
(defvar *logfile* NIL)

(defun start-log ()
  (setq *logfile*
	(open "agador.log"
	      :direction :output
	      :if-exists :supersede)))

(defun stop-log ()
  (close *logfile*))

;;; Use this in place of direct calls to FORMAT for simple logging.
(defun log (fmt &rest args)
  "Write something to the log file"
  (grab-mutex *logmtx*)

  ;; Include a timestamp HH:MM:SS
  (multiple-value-bind
	(sec min hour)
      (get-decoded-time)
    (format *logfile*
	    "~2,'0d:~2,'0d:~2,'0d " hr min sec))

  ;; Include the thread name
  (format *logile*
	  "[~a] "
	  (sb-thread:thread-name sb-thread:*current-thread*))

  ;; Now the message.
  (apply 'format *logfile* fmt args)

  ;; End line if not already
  (fresh-line *logfile*)
  (force-output *logfile*)
  (sb-thread:release-mutex *logmtx*))
