;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; A message-driven background thread server.

;;;; Usage:
;;;; (defun docmd (msg)
;;;;   (uiop:run-program msg :output '(:string :stripped T)))
;;;; (defvar *syscmd* (make-instance 'mbx-server :actor 'docmd))
;;;; (send *syscmd* "date")

(require :sb-concurrency)

(in-package :agu)

(defclass mbx-server () (
  (actor :initarg :actor :accessor actor)
  (name :initarg :name :accessor name :initform "noname")
  (queue :accessor queue :initform (sb-concurrency:make-mailbox))
  (mbthread :accessor server :initform NIL)
  ))

;;; Each mailbox has a processing thread running this function.
;;; When it receives a message it will call the designated action
;;; function.  It waits for that function to complete before
;;; looking for more messages for an 'actor'-like behavior.
(defun runmbx (e)
  (loop do
       (let ((msg (sb-concurrency:receive-message (queue e))))
	 (funcall (actor e) msg))))

;; If an actor function was specified, create a thread in which
;; 'runmbx executes.
(defmethod initialize-instance :after ((mbx mbx-server) &key)
  (when (actor mbx)
      (setf (server mbx)
	(sb-thread:make-thread
	 'runmbx
	 :name (name mbx)
	 :arguments (list mbx)))))

;; Exported API to put a message in the mailbox.
(defmethod send ((mbx mbx-server) msg)
  "Put a message in a mailbox."
  (sb-concurrency:send-message (queue mbx) msg))


