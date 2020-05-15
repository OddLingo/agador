;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;; Functions that display the contents of parser working trees.
(in-package :AGP)

;; Print the entire parse context so far, including all partials
(defun print-all ()
  "Print entire parser data"
  (loop for pos from 0 below (length *right*) do
       (format t "~%~2,'0d | " pos)
       (let ((col (elt *right* pos)))
	 (mapc (lambda (r) (print-object r t)) col)
	 )
       )
  NIL
  )

;; Print a single parser tree from top down.
(defun indent (sp)
  (loop for n from sp downto 0 do (format T " ") ))
(defgeneric print-tree (pterm &optional depth ))
(defmethod print-tree ((p ppair) &optional (depth 0))
  (indent depth)
  (print-object p T) (terpri)
  (print-tree (agc:left p) (+ depth 3))
  (print-tree (agc:right p) (+ depth 3))
  )
(defmethod print-tree ((u pusage) &optional (depth 0))
  (indent depth)
  (print-object u t )
  (terpri))

;; Paint graphic representation of a parse tree.
(defun dashes (n)
  (loop for x from 0 below n do (format T "-")))
(defun vline (x y1 y2)
  (loop for y from y1 to y2 do
       (agu:setxy x y)
       (format T "|")
       )
  )
(defun hline (y x1 x2)
  (agu:setxy x1 y)
  (loop for x from x1 to x2 do
       (format T "-")
       )
  )

(defun average (n1 n2) (floor (/ (+ n1 n2) 2 )))
(defun hpos (pos) (* pos 7))
(defgeneric paint-parse (pterm &optional depth top))
(defmethod paint-parse ((u pusage) &optional (depth 0) (top 1))
  (let ((xpos (hpos (term-lpos u))))
    (agu:setxy xpos top)
    (format T "~a" (agc:spelled u))
    (agu:setxy xpos (1+ top))
    (format T "~a" (agc:term-fn u))
    (+ depth top 1)
    ))
(defmethod paint-parse ((p ppair) &optional (depth 2) (top 1))
  (let* (
	 (lx (hpos (term-lpos p)))
	 (rx (+ 4 (hpos (term-rpos p))))
	 (ly (paint-parse (agc:left p) depth top))
	 (ry (paint-parse (agc:right p) depth top))
	 (base (+ top 2))
	 (py (max ly ry))
	 )
    (agu:set-color 3 0)
    (hline py lx rx)
    (vline lx base py)
    (vline rx base py)
    (agu:setxy (- (average lx rx) (1+ base)) py)
    (agu:set-color 0 3)
    (format T " ~a " (agc:term-fn p))
    (agu:set-color 7 0)
    (1+ py)
    ))
(defvar *leaves*)
;; Create a list of the terminal words in a tree.
(defun list-from-tree (m)
  (if (equal (type-of m) 'pusage)
      (push (agc:spelled m) *leaves*)
      (progn
	(list-from-tree (agc::right m))
	(list-from-tree (agc::left m))
	)
      )
    )

(defun string-from-tree (mt)
  (setq *leaves* NIL)
  (list-from-tree mt)
  (agu:string-from-list *leaves*)
  )
