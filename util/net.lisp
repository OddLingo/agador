;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :agu)

(require :sb-concurrency)

(defclass netport ()
  ((connection :initarg :cnx :accessor cnx)
   (outqueue :accessor outqueue)
   (inhandler :initarg :handler)
   (inthread :initarg ithread :accessor ithread)
   (outthread :initarg othread :accessor othread)
   ))


(defmethod disconnect ((np netport))
  (sb-thread:interrupt-thread (ithread np) 'alldone)
  )

(defmethod send ((np netport) msg)
  (sb-concurrency:send-message (outqueue np) msg)
  )

(defun netreceiver (np)
  (loop do
       (unwind-protect
	    (let ((msg (usocket:wait-for-input (cnx np))))
	      (funcall (handler np) msg np)
	      )
	 (usocket:socket-close (cnx np)))
       )
    )

;; This function loops sending messages.  It blocks when there is nothing
;; to send.
(defun netsender (np)
  (loop for msg = (sb-concurrency:receive-message (outqueue np))
     do
       (unwind-protect
	    (let ((stream (usocket:socket-stream (cnx np))))
	      (format stream  "~a" msg)
	      (force-output stream)
	      )
	 (usocket:socket-close (cnx np))
	 )
       )
  )

;; Create a TCP/IP connection and a thread to listen for incoming
;; messages.  Messages will be delivered to a handler function.
(defun connect (ip port handler)
  (let ((np (make-instance 'netport
	  :cnx (usocket:socket-connect ip port)
	  :ihandler handler)))
    ;; The threads are set separately so we can pass the NetPort
    (setf (ithread np)
	  (sb-thread:make-thread 'netreceiver :arguments (c)))
    (setf (othread np)
	  (sb-thread:make-thread 'netsender :arguments (c)))
    np)
  )
