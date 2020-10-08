;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

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


	   ;; ((equal verb "dt") (dump :TREE))
	   ;; ((equal verb "dc") (dump :CNTX))
	   ;; ((equal verb "dw") (agp:print-words))
	   ;; ((equal verb "v") (AGA::enable-action T))

