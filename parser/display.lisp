;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;; Functions that display the contents of parser working trees.
(in-package :AGP)

;;; Print the entire parse context so far, including all partials.
(defun print-all ()
  "Print entire parser data"
  (sb-thread:with-mutex (AGU::*tmtx*)
    (agu:clear)
    (loop for pos from 0 below (length *right*) do
	 (format t "~%~2,'0d | " pos)
	 (let ((col (elt *right* pos)))
	   (dolist (r col) (print-object r T)))))
  NIL)

;;; Print a single parser tree from top down.
(defun indent (sp)
  "Emit a number of spaces"
  (declare (type integer sp))
  (loop for n from sp downto 0 do (format T " ") ))

(defgeneric print-tree (pterm &optional depth ))
(defmethod print-tree ((p ppair) &optional (depth 0))
  (indent depth)
  (print-object p T) (terpri)
  (print-tree (agc:left p) (+ depth 3))
  (print-tree (agc:right p) (+ depth 3)))

(defmethod print-tree ((u pusage) &optional (depth 0))
  (indent depth)
  (print-object u t )
  (terpri))

;;; Create a list of the terminal words in a tree.
;;; There is a similar function in the Memory package,
;;; but it deals with remembered statements.
(defun list-from-tree (start)
  "List of the words in a parsed tree"
  (declare (type pterm start))
  (let ((leaves NIL))
    (labels
	((pleaves (m)
	   (case (type-of m)
	     (pusage (push
		       (agc:spelled m)
		       leaves))
	     (ppair (pleaves (agc::right m))
		    (pleaves (agc::left m))
		    ))))
      (pleaves start))))

(defun string-from-tree (mt)
  "String of the words in a parsed tree"
  (declare (type pterm mt))
  (agu:string-from-list (list-from-tree mt)))
