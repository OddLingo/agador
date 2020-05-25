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

(defun security-listener ()
  "Listener for UDP security messages."
  (let* ((socket (usocket:socket-connect nil nil
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:local-port 5399))
	 (buffer-size 32)
	 (in-buffer
	  (make-array 32 :element-type '(unsigned-byte 8))))

    (unwind-protect
	 (loop
	    (multiple-value-bind (buffer size)
		(usocket:socket-receive socket in-buffer buffer-size)
	      (security-alert (agm:bytes-to-s buffer) size)))
      (usocket:socket-close socket))))

(defun start-security ()
  (sb-thread:make-thread 'security-listener :name "Security"))
