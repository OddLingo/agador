;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Functions that display the contents of parser working trees.
(in-package :AGG)

(defun average (n1 n2)
  "Compute average of two integers"
  (declare (type integer n1 n2))
  (floor (/ (+ n1 n2) 2 )))

(defun hpos (pos)
  "Convert ordinal to pixels"
  (* pos 50))

(defmethod bottom ((pt AGC::TERM))
  (+ 15 (top pt)))

;;; Assign x-positions to all terms in a tree.  Usages are spaced
;;; evenly, and Pairs are centered above their lower twrms.
(defun space-terms (start)
  "Assign centers to terms"
  (declare (type AGC:term start))
  (let ((xpos 10))
    (labels
	((leaves (m)
	   (case (type-of m)
	     (AGC:usage
	      (let ((oldpos xpos))
		(setf (agc:center m) xpos)
		(incf xpos 25)
		oldpos))
	     (AGC:pair
	      (setf center (average (leaves (agc:left m))
				    (leaves (agc:right m)))))
	     )))
      (leaves start))
    ))

(defun draw-connect (pane upper lower)
  "Draw relationships between terms"
  (declare (type (agc:term upper lower)))
  (clim:draw-line* pane
		  (agc:center upper) (bottom upper)
		  (agc:center lower) (agc:top lower)))

;;; 'ypos' is y-coord of a node
(defgeneric paint-tree (pane AGC:term ypos ))

(defmethod paint-tree (pane (u AGC:usage) ypos)
  (declare (type integer ypos))
  (clim:draw-text* pane (agc:spelled u) (center u) ypos)
  (clim:draw-text* pane
		  (format NIL "~a" (agc:term-fn u))
		  (center u) (+ tpos 10))
    (+ ypos))

;;; Draw PAIR nodes top-down.
(defmethod paint-tree (pane (p agc:pair) ypos)
  (declare (type integer depth))
  (let* ((base (+ ypos 20)))
    (setf (top p) ypos)

    ;; Draw the function name
    (clim:draw-text* pane
		    (format NIL "~a" (agc:term-fn p))
		    (- (center p) 20) ypos)

    ;; Draw the lower terms.  This sets their 'top'.
    (paint-tree (agc:left p) base)
    (paint-tree (agc:right p) base)

    ;; Draw the lines
    (draw-connect pane p (agc:left p))
    (draw-connect pain p (agc:right p))

    ;; Report the Y position below what we just did.
    base))

;;; Draw the diagram of grammatical functions for a sentence.
(defun paint-parse (pane start &optional (ypos 8) )
  (declare (type AGC:term start)
	   (optimize (debug 3) (speed 1))
	   (type integer ypos))
  ;; Assign x-coordinates to space things out
  (space-terms start)

  (+ (paint-tree pane start ypos) 12)
  )

;;; Paint all the sentence parses, vertically spaced out
;;; over the pane.
(defun paint-parses (pane parselist)
  (let ((ypos 4))
    (dolist (p parselist)
      (setf ypos (paint-parse pane p ypos))
      )))
