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

;;;; Paint graphic representation of a parse tree.
(defun dashes (n)
  "Emit a number of dashes"
  (declare (type integer n))
  (loop for x from 0 below n do (format T "-")))

(defun vline (x y1 y2)
  "Draw a vertical line"
  (declare (type integer x y1 y2))
  (loop for y from y1 to y2 do
       (agu:setxy x y)
       (format T "|")))

(defun hline (y x1 x2)
  (declare (type integer y x1 x2))
  "Draw a horizontal line"
  (agu:setxy x1 y)
  (loop for x from x1 to x2 do
       (format T "-")))

(defun average (n1 n2)
  (declare (type integer n1 n2))
  (floor (/ (+ n1 n2) 2 )))

(defun hpos (pos) (* pos 8))

;;; 'depth' is how far down we are in the tree.  'top'
;;; is the y-position of the word line.
(defgeneric paint-tree (pterm &optional top depth ))

(defmethod paint-tree ((u pusage) &optional (top 1) (depth 0))
  (declare (type integer depth top))
  (declare (ignore depth))
  (let ((xpos (hpos (term-lpos u))))
    ;; Word text on top line
    (agu:setxy xpos top) (format T "~a" (agc:spelled u))
    ;; Function name just below
    (agu:setxy xpos (1+ top)) (format T "~a" (agc:term-fn u))
    (+ 2 top)))

;;; Draw PAIR nodes bottom-up.
(defmethod paint-tree ((p ppair) &optional (top 1) (depth 2))
  (declare (type integer depth top))
  (let* (
	 (left-x (hpos (term-lpos p)))
	 (right-x (+ 4 (hpos (term-rpos p))))
	 (nextrow (1+ depth))
	 ;; Draw lower nodes first.  Each reports how far
	 ;; down they went.
	 (left-y (paint-tree (agc:left p) top nextrow))
	 (right-y (paint-tree (agc:right p) top nextrow))
	 ;; Vertical lines start at base
	 (base (+ top 2))
	 ;; Where we will draw our line across
	 (pair-y (1+ (max left-y right-y))))
 
    ;; Draw the lines
    (agu:set-color 3 0)   ;; Yellow on black
    (hline pair-y left-x right-x)
    (vline left-x base pair-y)
    (vline right-x base pair-y)
    ;; Draw the function name
    (agu:setxy (- (average left-x right-x) 2) pair-y)
    (agu:set-color 0 3)   ;; Black on yellow
    (format T " ~a " (agc:term-fn p))
    (agu:set-color 7 0)   ;; Back to white on black
    ;; Report the Y position below what we just did.
    (1+ pair-y)))

;;; Draw the diagram of grammatical functions for a sentence.
(defun paint-parse (start &optional (top 1) )
  (declare (type pterm start)
	   (optimize (debug 3) (speed 1))
	   (type integer top))
  (sb-thread:with-mutex (AGU::*tmtx*)
    (agu:set-scroll NIL)
    (agu:setxy 1 top)
    (unless (= (prob start) 100)
      (format T "Probability ~D~%" (prob start))
      (incf top))
    (let ((newtop (paint-tree start top)))
      (agu:setxy 1 newtop)
      (agu:set-scroll T)
      (finish-output)
      (1+ newtop))))

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
