;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; This file decribes the overall layout of the program
;;;; user interface, using the McCLIM package.
(in-package :AGG)

(defvar *app* NIL)

(define-application-frame agador ()
  ;; Data slots
  ((input-text :initform "toki-pona" :accessor input-text)
   (output-text :initform "toki-pona" :accessor output-text)
   (cursor :initform NIL :accessor cursor)
   (contexts :initform NIL :accessor contexts)
   (current-parses :initform NIL :accessor current-parses))

  ;; The various panes within the window.
  (:menu-bar menubar-table)
  (:panes
   (intext
    :application :height 70 :width 400
    :background +light-goldenrod-yellow+
    :display-function 'show-intext)
   (outext
    :application :height 70 :width 400
    :background +LightCyan+
    :display-function 'show-outext)
   (context :application :height 300 :width 400
	    :display-function 'show-contexts)
   (syntax :application :height 360 :width 500
	   :display-function 'draw-parse)
   (int :interactor :height 40 :width 400)
   )

  (:layouts
   (default
       (horizontally ()
	   (vertically () intext outext context int)
	   syntax)))
  )

;;;; These are commands that the user can trigger.
(define-agador-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;; (define-agador-command (com-save :name t) ()
;;   (mcclim-raster-image:with-output-to-raster-image-file
;;       (stream "agador-toki.png")
;;     (show-intext *application-frame* stream)))

(define-agador-command (com-hear :name t) ((txt 'string))
  (agm:with-memory
      (setf (input-text *application-frame*) txt)
      (agp:parse-string txt)))

(define-agador-command (com-up :name T) ((ndx 'integer))
  "Move memory cursor up"
  (when (contexts *application-frame*)
    (agm:with-memory
	;; Select signature from list of contexts
	(goto (nth ndx (contexts *application-frame*))))))

(define-agador-command (com-left :name T) ()
  "Move memory cursor down to the left"
  (when (cursor *application-frame*)
    (agm:with-memory
	(goto (agc:right (cursor *application-frame*))))))

(define-agador-command (com-right :name T) ()
  "Move memory cursor down to the right"
  (when (cursor *application-frame*)
    (agm:with-memory
	(goto (agc:right (cursor *application-frame*))))))

(make-command-table 'menubar-table
		    :errorp NIL
		    :menu '(("Quit" :command com-quit)
;;			    ("Save" :command com-save)
			    ))


;;;; These functions update the displays of information.
(defun show-intext (frame pane)
  (show-text (input-text frame) pane))
(defun show-outext (frame pane)
  (show-text (output-text frame) pane))

(defun show-text (msg pane)
  (draw-text* pane msg 10 38
	      :text-family "linja pona"
	      :text-size 45
	      :ink +DarkBlue+)
  (draw-text* pane msg 10 60
	      :text-family "Bitstream Vera Serif"
	      :text-size 14)
 )

(defun show-contexts (frame pane)
  "Show contexts of the cursor"
  (let ((ctx (contexts frame)))
    (cond
      ((null ctx) T)
      ((equal (type-of ctx) 'cons)
       (loop for c in ctx for cnum from 0
	  do (format pane "~d: ~a"
		     cnum
		     (agm:string-from-tree c))))
      (T (format pane ctx)))))

;;; Change the memory browser context to the node with
;;; a given signature.
(defun goto (merk)
  (declare (type string merk))
  (declare (optimize (speed 2)(debug 3)))
  (let ((tree (agm:get-tree merk)))
    (setf (cursor *app*) tree)
    (setf (contexts *app*) (agm:get-context merk))
    (setf (current-parses *app*) tree)
  ))

(defun draw-parse (frame pane)
  "Repaint parse tree"
  (let ((p (current-parses frame)))
    (when p
      (log:info p)
      (agm:with-memory
	  (paint-parse pane p 10)))))

(defun run-window ()
  (setf *app* (make-application-frame 'agador))
  (run-frame-top-level *app*))

;;;; These events can be triggered by internal activity to change
;;;; the information being displayed.
(defclass new-text-event (clim:window-manager-event)
  ((msg :initarg :text :accessor text)))

(defclass new-status-event (clim:window-manager-event)
  ((msg :initarg :text :accessor text)))

(defclass new-output-event (clim:window-manager-event)
  ((msg :initarg :text :accessor text)))

(defclass new-parse-event (clim:window-manager-event)
  ((tree :initarg :tree :accessor tree)
   (context :initarg :context :accessor context)))

;;;; Handling an event usually requires an explicit
;;;; request to redisplay affected panes.
(defmethod handle-event ((frame agador) (event new-output-event))
  (setf (output-text *application-frame*) (text event))
  (redisplay-frame-pane frame 'outext))

(defmethod handle-event ((frame agador) (event new-text-event))
  (setf (input-text *application-frame*) (text event))
  (agm:with-memory
      (agp:parse-string (text event))))

(defmethod handle-event ((frame agador) (event new-status-event))
  (setf (contexts *application-frame*) (text event))
  (redisplay-frame-pane frame 'context))

(defmethod handle-event ((frame agador) (event new-parse-event))
  (setf (current-parses *application-frame*) (tree event))
  (setf (contexts *application-frame*) (context event))
  (redisplay-frame-pane frame 'syntax)
  (redisplay-frame-pane frame 'context))

;;;; The 'set' functions are the internal API for changing
;;;; the information displayed on the screen.  They generate
;;;; 'events' that are processed by the 'handlers' within
;;;; the McCLIM command loop.
(defun set-text (msg-text)
  "Change the displayed input text programmatically"
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-text-event
			       :sheet sheet
			       :msg msg-text)))
    (queue-event sheet event)))

(defun set-output (msg-text)
  "Change the displayed output text programmatically"
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-output-event
			       :sheet sheet
			       :text msg-text)))
    (queue-event sheet event)))

(defun set-parse (treetop &optional (ctx NIL))
  "API to change the parse tree diagram"
  (declare (optimize (speed 2)(debug 3)))
  (sb-debug:print-backtrace :count 5)
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-parse-event
			       :sheet *app*
			       :context ctx
			       :tree treetop)))
    (queue-event sheet event)))

(defun set-status (fmt &rest args)
  "Replace text on the status line"
  (let* ((sheet (frame-top-level-sheet *app*))
         (event (make-instance 'new-status-event
			:sheet sheet
			:text (apply 'format (list NIL fmt args)))))
    (queue-event sheet event)))
 
