;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; The "tree" database contains one record per term,
;;;; keyed by the node's Merkle signature.  Each stores a single
;;;; character string of space-separated words.  Fetching will
;;;; create an MPAIR or MUSAGE object from that string.
(defun get-tree (key)
  "Create memory object from the tree database"
;;  (declare (type string key))
  (declare (optimize (debug 3)(speed 1)))
  (let ((data (db-get :TREE key)))
    (when data
      (let* (
	     ;; Got a string.  Split into words.
	     (words (agu:words-from-string data))
	     ;; The record type is in the first word.
	     (record-type (char (first words) 0))
	     )
	(case record-type
	  ;; Usage nodes:  (#\u AGF:FN spelling)
	  (#\u (make-instance 'musage
			      :spelled (third words)
			      :fn (intern (second words) :AGF)))
	  ;; Pair nodes: (#\p AGF:FN lefthash righthash) 
	  (#\p (make-instance 'mpair
			      :fn (intern (second words) :AGF)
			      :left (third words)
			      :right (fourth words)))
	  ;; Anything else is an error.
	  (otherwise
	   (error "Bad record in TREE DB at ~a: ~a~%" key data)
	   NIL))))
    ))

;;;; The 'context' database stores a list of the signatures
;;;; of all the immediate parent nodes to the term whose
;;;; signature is the key.  This is used for navigating
;;;; up the tree.
(defun get-context (key)
  "Get list of contexts for a term"
  (declare (type string key))
  (let ((data (db-get :CNTX key)))
    (if data
	(agu:words-from-string data)
	NIL)))

;;; Add a new context above a node, avoiding duplicates.
(defun add-context (child parent)
;;  (declare (type string child parent))
  (let ((c (db-get :CNTX child)))
    (if c
	;; Child already has contexts - check for duplicates.
	(let ((previous (agu:words-from-string c)))
	  (unless (member parent previous)
		(push parent previous)
		(db-put :CNTX child
			(agu:string-from-list previous))))

	;; Else it is the first context for this child.
	(db-put :CNTX child parent)
	)))

;;; The 'string-representation' function takes care of
;;; creating the proper stored representation of the data.
;;; The key is then the hash of that.
(defun put-tree (mt)
  (declare (mterm mt))
  (db-put :TREE (sig mt) (store mt)))

;;; Clone a parser tree structure into long-term memory.  Note that
;;; pairs are cloned recursively.  The value returned is always the
;;; Merkle key of the remembered object.  
(defgeneric remember (pterm))
(defmethod remember ((u agp:pusage))
  "Remember a word usage"
  (declare (optimize (debug 3)(speed 1)))
  (let ((m (make-instance 'musage
	:fn (agc:term-fn u)
	:spelled (agc:spelled u))))
    ; If it does not already exist, create it in the db.
    (unless (get-tree (sig m)) (put-tree m))	  
    (sig m)
    ))

(defmethod remember ((p agp:ppair))
  "Remember a syntax pair"
  (declare (optimize (debug 3)(speed 1)))
  (log:info p)
  (let* ((left-child  (remember (agc:left p)))
	 (right-child (remember (agc:right p)))
	 (m (make-instance 'mpair
			   :fn (agc:term-fn p)
			   :left  left-child
			   :right right-child))
	 (pair-sig (sig m)))

    ;; If this exact pair is not already in the db, create it
    ;; and the contexts up from the lower nodes.
    (unless (get-tree pair-sig)
      (put-tree m)
      (add-context left-child  pair-sig)
      (add-context right-child pair-sig))
    pair-sig))
(defmethod remember ((s string)) s)

;;;; Recalling is the inverse of remembering.  We use the same
;;;; recursive descent and ascent, but do not create anything
;;;; that is missing.
(defgeneric recall-p (agp:pterm))
(defmethod recall-p ((node agp:pusage))
  (get-tree (merkle node)))
(defmethod recall-p ((p agp:ppair))
  (unless (recall-p (agc:left p)) (return-from recall-p NIL))
  (unless (recall-p (agc:right p)) (return-from recall-p NIL))
  (get-tree (merkle p)))

(defmethod recall-p ((s string)) s)

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
