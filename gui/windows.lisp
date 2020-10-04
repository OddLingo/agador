;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This file decribes the overall layout of the program
;;;; user interface.
(in-package :AGG)

(define-application-frame agador ()
  ;; Data slots
  ((current-text :initform "toki-pona" :accessor current-text)
   (current-parse :initform NIL :accessor current-parse))

  ;; The various panes wihtin the window.
  (:menu-bar menubar-table)
  (:panes
   (latin
    :application
    :text-style (make-text-style NIL NIL 16)
    :height 20 :width 400
    :display-function 'show-current)
   (linja
    :application
    :text-style (make-text-style "linja pona" NIL 40)
    :height 40 :width 400
    :background +light-goldenrod-yellow+
    :foreground +DarkBlue+
    :display-function 'show-current)
   (context :application :height 300 :width 400)
   (syntax :application :height 360 :width 400
	   :display-function 'draw-parse)
   (int :interactor :height 40 :width 400)
   )

  (:layouts
   (default
       (horizontally ()
	   (vertically () linja latin context int)
	   syntax)))
  )

(define-agador-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-agador-command (com-hear :name t) ((txt 'string))
  (setf (current-text *application-frame*) txt))

(make-command-table 'menubar-table
		    :errorp NIL
		    :menu '(("Quit" :command com-quit)
			    ))

(defun show-current (frame pane)
  (format pane "~a~%" (current-text frame)))

(defun draw-parse (frame pane)
  (clim:draw-line* pane 0 0 100 100)
)

(defvar *app* (make-application-frame 'agador))

(defun run-window ()
  (run-frame-top-level *app*))

(defclass new-text-event (clim:window-manager-event)
  ((msg :initarg :text :accessor text)))

(defmethod handle-event ((frame agador) (event new-text-event))
  (setf (current-text *application-frame*) (text event)))

(defun set-text (msg-text)
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-text-event
			       :sheet agador
			       :msg msg-text)))
    (queue-event sheet event)))
