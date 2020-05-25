;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; A background thread that listens for messages coming from the
;;;; surveillance camera system.

(in-package :AGA)

(defun security-alert (buffer)
  "Handle security messages."
  (cond
    ((equal buffer "MAIL") (ags:say "The mail has arrived."))
    ((equal buffer "GARB") (ags:say "The trash has been collected."))
    (T (agu:term "Unexpected security message ~a~%" buffer))))

(defun security-listener ()
  "Listener for UDP security messages."
  (let* ((socket (usocket:socket-connect nil nil
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:local-port 5399))
	 (buffer-size 32)
	 (in-buffer (make-vector)))

    (unwind-protect
	 (loop
	    (multiple-value-bind (buffer)
		(usocket:socket-receive socket in-buffer buffer-size)
	      (security-alert (agm:bytes-to-s buffer))))
      (usocket:socket-close socket))))

(defun start-security ()
  (sb-thread:make-thread 'security-listener :name "Security"))
