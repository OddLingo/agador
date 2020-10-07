;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Functions that display the contents of syntax trees.
(in-package :AGG)

(defun average (n1 n2)
  "Compute average of two integers"
  (declare (type integer n1 n2))
  (floor (/ (+ n1 n2) 2 )))

(defun hpos (pos)
  "Convert ordinal to pixels"
  (* pos 50))

(defgeneric bottom (AGC:TERM))
(defmethod bottom ((pt AGC::PAIR))
  (+ 12 (agc:top pt)))
(defmethod bottom ((pt AGC::USAGE))
  (+ 20 (agc:top pt)))

;;; Assign x-positions to all terms in a tree.  Usages are spaced
;;; evenly, and Pairs are centered above their lower terms.
(defun space-terms (start)
  "Assign centers to terms"
  (declare (type AGC:term start))
  (let ((xpos 25))
    (labels
	((leaves (m)
	   (case (type-of m)
	     (AGP:pusage
	      (let ((oldpos xpos))
		(setf (agc:center m) xpos)
		(log:info "~a centered at ~D" m xpos)
		;; Horizontal pixels between words.
		(incf xpos 70)
		oldpos))
 
	     (AGP:ppair
	      (setf (agc:center m)
		    (average (leaves (agc:left m))
			     (leaves (agc:right m))))
	      (log:info "~a centered at ~D" m (agc:center m))
	      (agc:center m))

	     (otherwise
	      (log:info "~a" (type-of m))
	      xpos)
	     )))
      ;; Descend the tree
      (leaves start))
    ))

(defun draw-connect (pane upper lower)
  "Draw relationships between terms"
  (declare (type agc:term upper lower))
  (clim:draw-line* pane
		  (agc:center upper) (bottom upper)
		  (agc:center lower) (agc:top lower)))

;;; 'ypos' is y-coord of a node
(defgeneric paint-tree (pane AGC:term ypos ))

(defmethod paint-tree (pane (u AGC:usage) ypos)
  (declare (type integer ypos)
	   (optimize (debug 3)(speed 1)))
  (let ((indent (- (agc:center u) 14)))
    (setf (agc:top u) ypos)
    (clim:draw-text* pane
		  (format NIL "~a" (agc:term-fn u))
		  indent (+ ypos 12))
    (clim:draw-text* pane
		   (agc:spelled u)
		   indent (+ ypos 24)))
    (+ 20 ypos))

;;; Draw PAIR nodes top-down.
(defmethod paint-tree (pane (p agc:pair) ypos)
  (declare (type integer ypos)
	   (optimize (debug 3)(speed 1)))
  (let ((base (+ ypos 30))
	(lowest 0))
    (setf (agc:top p) ypos)
    (log:info "~a in ~a at ~D base ~D center ~a"
	      p pane ypos base (agc:center p))

    ;; Draw the function name
    (clim:draw-text* pane
		    (format NIL "~a" (agc:term-fn p))
		    (- (agc:center p) 20) (+ ypos 12))

    ;; Draw the lower terms.  This sets their 'top'.
    ;; They also report back up their lowest extent.
    (setf lowest
	  (max
	   (paint-tree pane (agc:left p) base)
	   (paint-tree pane (agc:right p) base)))

    ;; Draw the lines
    (draw-connect pane p (agc:left p))
    (draw-connect pane p (agc:right p))

    ;; Report the Y position below what we just did.
    lowest))

;;; Draw the diagram of grammatical functions for a sentence.
(defun paint-parse (pane start &optional (ypos 8) )
  (declare (type AGC:term start)
	   (optimize (debug 3) (speed 1))
	   (type integer ypos))
  (log:info "Painting at ~D" ypos)
  ;; Assign x-coordinates to space things out
  (space-terms start)
  (log:info "Spacing done")
  (+ (paint-tree pane start ypos) 12)
  )

;;; Paint all the sentence parses, vertically spaced out
;;; over the pane.
(defun paint-parses (pane parselist)
  "Draw all available parses"
  (let ((ypos 10))
    (dolist (p parselist)
      (setf ypos (paint-parse pane p ypos))
      )))
