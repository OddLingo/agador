;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This file decribes the overall layout of the program
;;;; user interface, using the McCLIM package.
(in-package :AGG)

(defvar *app* NIL)

(define-application-frame agador ()
  ;; Data slots
  ((current-text :initform "toki-pona" :accessor current-text)
   (current-parses :initform NIL :accessor current-parses))

  ;; The various panes within the window.
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
   (syntax :application :height 360 :width 500
	   :display-function 'draw-parse)
   (int :interactor :height 40 :width 400)
   )

  (:layouts
   (default
       (horizontally ()
	   (vertically () linja latin context int)
	   syntax)))
  )

;;;; These are commands that the user can trigger.
(define-agador-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-agador-command (com-hear :name t) ((txt 'string))
  (setf (current-text *application-frame*) txt)
  (agm:db-start)
  (agp:parse-string txt)
  (agm:db-commit))

(make-command-table 'menubar-table
		    :errorp NIL
		    :menu '(("Quit" :command com-quit)
			    ))

;;;; These functions update the displays of information.
(defun show-current (frame pane)
  (format pane "~a~%" (current-text frame)))

(defun draw-parse (frame pane)
  "Repaint parse tree"
  (if (current-parses frame)
      (paint-parses pane (current-parses frame))
      ;; Draw a diagonal line if nothing to display.
      (clim:draw-line* pane 0 0 200 200
		       :line-thickness 10
		       :ink +red+)))

(defun run-window ()
  (setf *app* (make-application-frame 'agador))
  (run-frame-top-level *app*))

;;;; These events can be triggered by internal activity to change
;;;; the information being displayed.
(defclass new-text-event (clim:window-manager-event)
  ((msg :initarg :text :accessor text)))

(defclass new-parse-event (clim:window-manager-event)
  ((tree :initarg :tree :accessor tree)))

(defmethod handle-event ((frame agador) (event new-text-event))
  (setf (current-text *application-frame*) (text event)))

(defmethod handle-event ((frame agador) (event new-parse-event))
  (setf (current-parses *application-frame*) (tree event)))

(defun set-text (msg-text)
  "Change the displayed text programmatically"
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-text-event
			       :sheet sheet
			       :msg msg-text)))
    (queue-event sheet event)))

(defun set-parse (treetop)
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-parse-event
			       :sheet sheet
			       :tree treetop)))
    (queue-event sheet event)))
