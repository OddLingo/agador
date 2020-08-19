;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

(defvar *cursor* NIL)
(defvar *contexts* NIL)

(defclass uimode ()
  (
   (voice :initform NIL :accessor voice)
   )
  )

(defvar *uimode* (make-instance 'uimode))

;; Setting this to true will turn on speech recognition.
(defun set-voice (yes)
  (setf (voice *uimode*) yes)
  (if yes
	(ags:jsend "RESUME")
	(ags:jsend "PAUSE")))

;;; Create a list of the terminal words in a tree.  We descend the
;;; tree right-side-first but are pushing it onto the list of leaves.
;;; This results in the final list being in the correct order
;;; left-to-right.
(defun list-from-tree (start)
  (declare (type string start))
  (let ((leaves NIL))
    (labels
	((find-leaf (mt)
	   (let* ((m (get-tree mt))
		 (ty (type-of m)))
	     (case ty
	       (musage (push (agc:spelled m) leaves))
	       (mpair
		(progn
		  (find-leaf (agc::right m))
		  (find-leaf (agc::left m))))
	       (integer
		(push
		  (if (> m 100)
		      (aga:speakable-time m)
		      (format NIL "~d" m))
		  leaves))))))
      (find-leaf start))
    leaves))

;;; Get a single string of the spelling of the words in a tree.
(defun string-from-tree (mt)
  (agu:string-from-list (list-from-tree mt)))

(defun prompt () (agu:term "~%-> "))

;; Repaint the screen with the current text at the top, followed
;; by the contexts it appears in.
(defun repaint ()
  (agu:use-term)
  (agu:clear)
  ;; Context lines in white
  (agu:set-color 7 0)
  (loop for c in *contexts* for cnum from 0
    do (format T "~d: ~a~%" cnum (string-from-tree c)))

  ;; Highlight focus line in yellow
  (agu:set-color 0 3)
  (format T "~%~C[K" (code-char 27))
  (if (null *cursor*)
      (progn
	(format T "Type something to set context~%~%"))
      (progn
	(format T "~a  ~a~%~%"
	  (agc:term-fn *cursor*)
	  (string-from-tree (sig *cursor*)))))

  ;; Prompt in white on black.
  (agu:set-color 7 0)
  (agu:release-term)
)

;; Change the explore context to the node with a given signature.
(defun goto (s)
  (format T "Exploring from ~a~%" s)
  (if s (progn
	  (setq *cursor* (get-tree s))
	  (setq *contexts* (get-context s))
	  )
      (setq *cursor* NIL))
  (repaint)
  )

;; Top loop for exploring the long-term memory.
(defun explore ()
  (agu:clear)
  (agu:set-scroll)
  (agu:term "Type something to set context~%")
  (prompt)
  (loop for line = (read-line)
     until (equal line "x")
     when (> (length line) 0)
     do
       ;; A transaction around each command.
       (db-start)
       (let* ((verb line)
	      (cmd (char-code (char verb 0)))
	      )
	 (cond
	   ((and (>= cmd 48) (<= cmd 57))
	    (goto (nth (- cmd 48) *contexts*)))
	   
	   ((equal verb "l") ;; l
	    (if (eq (type-of *cursor*) 'mpair)
		(goto (agc:left *cursor*))
		(format T "Can't do that here~%")))

	   ((equal verb "r") ;; r
	    (if (eq (type-of *cursor*) 'mpair)
		(goto (agc:right *cursor*))
		(format T "Can't do that here~%")))

	   ((equal verb "dt") ;; Dump tree
	    (dump :TREE))
	   ((equal verb "dc") ;; Dump contexts
	    (dump :CNTX))
	   ((equal verb "dw") ;; Dump words
	    (agp:print-words))
	   ((equal verb "v") (set-voice T))

	   (T
	    (let ((r (agp:parse-words (agu:words-from-string line))))
	      (if r (goto r) (prompt))))

	   ))
       (prompt)
       (db-commit)
       )
  )

	   
