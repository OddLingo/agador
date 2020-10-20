;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; Functions that display the contents of syntax trees.
(in-package :AGG)

(defun average (n1 n2)
  "Compute average of two integers"
  (declare (type integer n1 n2))
  (floor (/ (+ n1 n2) 2 )))

(defgeneric bottom (AGC:TERM))
(defmethod bottom ((pt AGC:PAIR))  (+ 12 (agc:top pt)))
(defmethod bottom ((pt AGC:USAGE)) (+ 20 (agc:top pt)))

;;; Assign x-positions to all terms in a tree.  Usages are spaced
;;; evenly, and Pairs are centered above their lower terms.
(defun space-terms (start)
  "Assign centers to terms"
  (declare (type AGC:term start))
  (let ((xpos 25))  ;; Horizontal pixels between words.
    (labels
	((leaves (m)
	   (cond
	     ((subtypep (type-of m) 'AGC:usage)
	      (let ((oldpos xpos))
		(setf (agc:center m) xpos)
		(incf xpos 60)
		oldpos))
 
	     ((subtypep (type-of m) 'AGC:pair)
	      (setf (agc:center m)
		    (average (leaves (agc:left m))
			     (leaves (agc:right m))))
	      (agc:center m))

	     (T (progn
		  (log:info "~a" (type-of m))
		  xpos))
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
  "Draw a 'usage' leaf node"
  (declare (type integer ypos))
  (let ((indent (- (agc:center u) 14)))
    (setf (agc:top u) ypos)
    (clim:draw-text* pane
		  (string (agc:term-fn u))
		  indent (+ ypos 12)
		  :text-family "Courier 10 Pitch"
		  :text-face "Bold"
		  )
    (clim:draw-text* pane
		   (agc:spelled u)
		   indent (+ ypos 28)
		   :text-family "Bitstream Vera Serif")
		  )
    (+ 20 ypos))


(defgeneric recalled (node))
(defmethod recalled ((node agc:pair)) node)
(defmethod recalled ((node agc:usage)) node)
(defmethod recalled ((node string)) (agm:get-tree node))

(defmethod paint-tree (pane (handle string) (ypos integer))
  (paint-tree pane (agm:get-tree handle) ypos))

;;; Draw PAIR nodes top-down.
(defmethod paint-tree (pane (p agc:pair) ypos)
  "Draw a 'pair' inner node"
  (declare (type integer ypos))
  (let ((base (+ ypos 30))
	(lowest 0))
    (setf (agc:top p) ypos)

    ;; Draw the function name
    (clim:draw-text* pane
		    (string (agc:term-fn p))
		    (- (agc:center p) 20) (+ ypos 12)
		    :text-family "Courier 10 Pitch"
		    :text-face "Bold"
		    )

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
(defgeneric paint-parse (pane start ypos))
(defmethod paint-parse (pane (start AGC:term) (ypos integer))
  "Draw one syntax tree"
  (declare (optimize (speed 1)(debug 3)))
  ;; Assign x-coordinates to space things out
  (space-terms start)
  (+ (paint-tree pane start ypos) 12))
(defmethod paint-parse (pane (parselist cons) (ypos integer))
  "Draw all available syntax trees"
  (let ((ypos 10))
    (dolist (p parselist)
      (setf ypos (paint-parse pane p ypos)))
    ypos))
(defmethod paint-parse (pane (empty NULL) ypos)
  (declare (ignore empty))
  (clim:draw-line* pane 0 0 200 200
		   :line-thickness 8
		   :ink +red+)
  ypos)
