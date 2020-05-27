;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; A background thread that listens for messages coming from the
;;;; surveillance camera system.

(in-package :AGA)

(defun security-alert (buffer size)
  "Handle security messages."
  (let ((msg (subseq buffer 0 size)))
    (cond
      ((equal msg "TMAIL") (ags:say "The mail has arrived."))
      ((equal msg "TGARB") (ags:say "The trash is being collected."))
      ((equal msg "TDWAY") (ags:say "We have a visitor."))
      ((equal msg "TWALK") (ags:say "People are walking by."))
      ((equal msg "TPKG") (ags:say "A package is being delivered."))
      (T (agu:term "Unexpected ~d byte ~a message ~a~%"
		 size (type-of msg) msg)))))

;;; Try to listen to the UDP port.  In case of quick restarts, it
;;; might still be bound, so we delay a bit and retry.
(defun try-connect ()
  "Retry port listen"
  (let ((sock NIL))
    (loop until sock do
	 (setf sock (usocket:socket-connect nil nil
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:local-port 5399))
	 (unless sock
	   (agu:term "Retry UDP listen~%")
	   (sleep 15)))
    sock))

;;; This is the top level of the Security thread.
(defun security-listener ()
  "Listener for UDP security messages."
  (let ((socket (try-connect))
	(buffer-size 32)
	(in-buffer
	  (make-array 32 :element-type '(unsigned-byte 8))))

    ;; Here we loop forever, waiting for incoming messages.
    (unwind-protect
	 (loop
	    (multiple-value-bind (buffer size)
		(usocket:socket-receive socket in-buffer buffer-size)
	      (security-alert (agm:bytes-to-s buffer) size)))
      (usocket:socket-close socket))))

(defun start-security ()
  "Start the security listener thread"
  (sb-thread:make-thread 'security-listener :name "Security"))
