;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This file decribes the overall layout of the program
;;;; user interface, using the McCLIM package.
(in-package :AGG)

(defvar *app* NIL)

(define-application-frame agador ()
  ;; Data slots
  ((current-text :initform "toki-pona" :accessor current-text)
   (cursor :initform NIL :accessor cursor)
   (contexts :initform NIL :accessor contexts)
   (current-parses :initform NIL :accessor current-parses))

  ;; The various panes within the window.
  (:menu-bar menubar-table)
  (:panes
   (linja
    :application
    :text-style (make-text-style "linja pona" NIL 40)
    :height 70 :width 400
    :background +light-goldenrod-yellow+
    :display-function 'show-current)
   (context :application :height 300 :width 400
	    :display-function 'show-contexts)
   (syntax :application :height 360 :width 500
	   :display-function 'draw-parse)
   (int :interactor :height 40 :width 400)
   )

  (:layouts
   (default
       (horizontally ()
	   (vertically () linja context int)
	   syntax)))
  )

;;;; These are commands that the user can trigger.
(define-agador-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;; (define-agador-command (com-save :name t) ()
;;   (mcclim-raster-image:with-output-to-raster-image-file
;;       (stream "agador-toki.png")
;;     (show-current *application-frame* stream)))

(define-agador-command (com-hear :name t) ((txt 'string))
  (agm:with-memory
      (setf (current-text *application-frame*) txt)
      (agp:parse-string txt)))

(define-agador-command (com-up :name T) ((ndx 'integer))
  (when (contexts *application-frame*)
    (agm:with-memory
	(goto (nth ndx (contexts *application-frame*))))))

(define-agador-command (com-left :name T) ()
  (when (cursor *application-frame*)
    (agm:with-memory
	(goto (agc:right (cursor *application-frame*))))))

(define-agador-command (com-right :name T) ()
  (when (cursor *application-frame*)
    (agm:with-memory
	(goto (agc:right (cursor *application-frame*))))))

(make-command-table 'menubar-table
		    :errorp NIL
		    :menu '(("Quit" :command com-quit)
;;			    ("Save" :command com-save)
			    ))

;;;; These functions update the displays of information.
(defun show-current (frame pane)
  (draw-text* pane (current-text frame)
	      10 38
	      :text-family "linja pona"
	      :text-size 45
	      :ink +DarkBlue+)
  (draw-text* pane (current-text frame)
	      10 60
	      :text-family "Bitstream Vera Serif"
	      :text-size 14)
 )

(defun show-contexts (frame pane)
  "Show contexts of the cursor"
    (loop for c in (contexts frame) for cnum from 0
       do (format pane "~d: ~a"
		  cnum
		  (agm:string-from-tree c)))
  )

;;; Change the memory browser context to the node with
;;; a given signature.
(defun goto (s)
  (declare (type string s))
  (setf (cursor *app*) (agm:get-tree s))
  (setf (contexts *app*) (agm:get-context s))
  (setf (current-parses *app*) (list (agm:get-tree s)))
  )

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
  (setf (current-text *application-frame*) (text event))
  (agm:with-memory
      (agp:parse-string (text event))))

(defmethod handle-event ((frame agador) (event new-parse-event))
  (setf (current-parses *application-frame*) (tree event))
  (redisplay-frame-pane frame 'syntax))

(defun set-text (msg-text)
  "Change the displayed text programmatically"
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-text-event
			       :sheet sheet
			       :msg msg-text)))
    (queue-event sheet event)))

(defun set-parse (treetop)
  "API for changing the parse tree list"
  (log:info "sending event with ~a" treetop)
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-parse-event
			       :sheet *app*
			       :tree treetop)))
    (queue-event sheet event)))

