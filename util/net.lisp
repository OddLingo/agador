;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Interface to TCP/IP network connections.

(in-package :agu)

(require :sb-concurrency)

(defclass netport ()
  ((connection :initarg :cnx :accessor cnx)
   (name :initarg :name :accessor name :initform "Listener")
   (inhandler :initarg :ihandler :accessor handler)
   (inthread :initarg :ithread :accessor ithread)
   ))

(defmethod disconnect ((np netport))
  (sb-thread:interrupt-thread (ithread np) 'alldone)
  )

(defmethod send ((np netport) msg)
  (unwind-protect
       (let ((stream (usocket:socket-stream (cnx np))))
	 (write-sequence msg stream)
	 (force-output stream)
	 )
    NIL
  ))

(defun netreceiver (np)
  (format T "Starting listener for ~a~%" (name np))
  (let* ((sock (cnx np))
	(stream (usocket:socket-stream sock)))
    (loop do
       (unwind-protect
	    (progn
	      (usocket:wait-for-input sock)
	      (funcall (handler np) (read-line stream) np)
	      )
	 NIL
	 )
    )
   ))

;; Create a TCP/IP connection and a thread to listen for incoming
;; messages.  Messages will be delivered to a handler function.
(defun connect (ip port &key handler name)
  (let ((np (make-instance 'netport
	  :name name
	  :cnx (usocket:socket-connect ip port)
	  :ihandler handler)))

    ;; I fthere is an input handler, we create a mailbox for it.
    (if handler
	(setf (ithread np)
	  (sb-thread:make-thread
	   'netreceiver
	   :name name
	   :arguments (list np))))
    np)
  )
