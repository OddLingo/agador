;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :agu)

(require :sb-concurrency)

(defclass netport ()
  ((connection :initarg cnx)
   (outqueue :accessor outqueue)
   (inhandler :initarg handler)
   (inthread :initarg ithread :accessor ithread)
   (outthread :initarg othread)
   ))

(defmethod disconnect ((np netport))
  (sb-thread:interrupt-thread (ithread np))
  )

(defun netreceiver ()
  (loop do
       (unwind-protect
	    (progn
	      (usocket:wait-for-input *cnx*)
	      (format t "Input is: ~a~%" (read-line *cnx*)))
	 (usocket:socket-close *cnx*))
       )
    )

;; This function loops sending messages.  It blocks when there is nothing
;; to send.
(defun netsender ()
  (loop for msg = (sb-concurrency:receive-message *outbound*)
     do
       (unwind-protect
	    (progn
	      (let ((stream (usocket:socket-stream *cnx*)))
		(format stream  "~a" msg)
		(force-output stream)
		)
	      )
	 (usocket:socket-close *cnx*))
       )
  )

(defun connect (ip port)
  (make-instance 'netport
		 :cnx (usocket:socket-connect ip port)
		 :ithread (sb-thread:make-thread 'msgreceiver)
		 :othread (sb-thread:make-thread 'msgsender)
		 )
  )


