;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

(defvar *cursor* NIL)
(defvar *contexts* NIL)

;;; Create a list of the terminal words in a memory tree.
;;; We descend the tree right-side-first but are pushing it
;;; onto the list of leaves.  This results in the final
;;; list being in the correct order left-to-right.
(defun list-from-tree (start)
  "Flatten memorized tree to a list"
  (declare (type string start))
  (let ((leaves NIL))
    (labels
	((find-leaf (mt)
	   (let* ((m (get-tree mt))
		 (ty (type-of m)))
	     (case ty
	       (MUSAGE (push (agc:spelled m) leaves))
	       (MPAIR
		(progn
		  (find-leaf (agc::right m))
		  (find-leaf (agc::left m))))
	       ))))
      (find-leaf start))
    leaves))

;;; Get a single string of the spelling of the words in a tree.
(defun string-from-tree (mt)
  (agu:string-from-list (list-from-tree mt)))

(defun prompt () (agu:term "~%-> "))

;; Repaint the screen with the current text at the top, followed
;; by the contexts it appears in.
(defun repaint ()
  (sb-thread:with-mutex (agu::*tmtx*)
    (log:info "~a with ~a" *cursor* *contexts*)
    ;; Clear below the diagram and set cursor there.
    (agu:clear AGU::+rtop+)
    (format T "=======~%")
    ;; Context lines in white
    (agu:set-color 7 0)
    (loop for c in *contexts* for cnum from 0
       do (format T "~d: ~a~%" cnum (string-from-tree c)))

    ;; Focus line in black on yellow
    (agu:set-color 0 3)
    (agu:clear-eol)
    (if *cursor*
	(progn
	  (format T "~a  ~a~%~%"
		  (agc:term-fn *cursor*)
		  (string-from-tree (sig *cursor*))))
	(progn
	  (log:info "Cursor NIL")
	  (format T "no cursor")))

    ;; Prompt in white on black.
    (agu:set-color 7 0))
)

;;; Change the explore context to the node with a given
;;; signature.
(defun goto (s)
  (log:info "Exploring from ~a" s)
  (if s
      (progn
	(setq *cursor* (get-tree s))
	(setq *contexts* (get-context s)))
      (setq *cursor* NIL))
  (repaint)
  )

;; Top loop for exploring the long-term memory.
(defun explore ()
  (agu:clear AGU::+rtop+)
  (agu:set-scroll)
  (agu:term "Type something to set context~%")
  (prompt)

  ;; Read commands
  (loop for line = (read-line)
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

	   ((equal verb "quit") ;; Stop
	    (progn
	      (agm:db-commit)
	      (return-from explore)))

	   ((equal verb "dt") ;; Dump tree
	    (dump :TREE))
	   ((equal verb "dc") ;; Dump contexts
	    (dump :CNTX))
	   ((equal verb "dw") ;; Dump words
	    (agp:print-words))
	   ((equal verb "v") (AGA::enable-action T))

	   ;; Anything else is a new statement to analyze.
	   ((>= (length line) 6)
	    (let ((r (agp:parse-words
		      (agu:words-from-string line))))
	      (when r (goto r))))

	   (T (agu:term "?~%"))
	   ))
       (prompt)
       (db-commit)
       )
  )

	   
