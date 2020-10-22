;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGM)

;;;; The "tree" database contains one record per term,
;;;; keyed by the node's Merkle signature.  Each stores a single
;;;; character string of space-separated words.  Fetching will
;;;; create a PAIR or USAGE object from that string.

;;; Creates the merkle hash for generating all keys in the 'tree' and
;;; 'context' databases.  We use the high-order 40 bits of a SHA1.
(defun hash-of (v)
  (declare (type string v))
  (subseq (sha1:sha1-hex v) 0 10))

;;; Reconstruct a syntax tree object from its key.  This is
;;; recursive and will rebuild the entire tree below that point.
(defun get-tree (key)
  "Create tree object from the long term memory"
  (declare (type string key))
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
	  (#\u (make-instance 'agc:usage
			      :spelled (third words)
			      :fn (intern (second words) :AGF)))
	  ;; Pair nodes: (#\p AGF:FN lefthash righthash) 
	  (#\p (make-instance 'agc:pair
			      :fn (intern (second words) :AGF)
			      :left (get-tree (third words))
			      :right (get-tree (fourth words))))
	  ;; Anything else is an error.
	  (otherwise
	   (error "Bad record in TREE DB at ~a: ~a~%" key data)
	   NIL))))
    ))

;;;; The 'context' database stores a list of the keys
;;;; of all the immediate parent nodes to the term whose
;;;; signature is the key.  This is used for navigating
;;;; up the tree.
(defun get-context (key)
  "Get list of contexts for a term"
  (declare (type string key))
  (let ((data (db-get :CNTX key)))
    (when data
      (agu:words-from-string data))))

;;; Add a new context above a node, avoiding duplicates.
(defun add-context (low-key high-key)
  (declare (type string low-key high-key))
  (let ((c (db-get :CNTX low-key)))
    (if c
	;; Child already has contexts - check for duplicates.
	(let ((previous (agu:words-from-string c)))
	  (unless (member high-key previous)
		(push high-key previous)
		(db-put :CNTX low-key
			(agu:string-from-list previous))))

	;; Else it is the first context for this child.
	(db-put :CNTX low-key high-key)
	)))

;;; Each tree node type has a string representation, which is
;;; what gets stored.  The key is then the hash of that.
;;; Note that pairs are cloned recursively.  The value
;;; returned is always the Merkle key of the remembered object.
;;; Passing :STORE NIL will return the key without actually storing.
(defgeneric remember (pterm &key store))
(defmethod remember ((u agc:usage) &key (store T))
  "Remember a word usage"
  (let* ((stored-as (format NIL "u ~a ~a"
			   (agc:term-fn u)
			   (agc:spelled u)))
	(key (hash-of stored-as)))

    ;; If it does not already exist, create it in the db.
    (when store
      (unless (db-get :TREE key)
	(db-put :TREE key stored-as)))
    key))

(defmethod remember ((p agc:pair) &key (store T))
  "Remember a syntax pair"
  (declare (optimize (debug 3)(speed 1)))
  (let* ((left-child  (remember (agc:left p) :STORE store))
	 (right-child (remember (agc:right p) :STORE store))
	 (stored-as (format NIL "p ~a ~a ~a"
			    (agc:term-fn p)
			    left-child
			    right-child))
	 (key (hash-of stored-as)))
    
    ;; If this exact pair is not already in the db, create it
    ;; and the contexts up from the lower nodes.
    (when store
      (unless (db-get :TREE key)
	(db-put :TREE key stored-as)
	(add-context left-child  key)
	(add-context right-child key)))
    key))

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
	       (AGC:USAGE (push (agc:spelled m) leaves))
	       (AGC:PAIR
		(progn
		  (find-leaf (agc::right m))
		  (find-leaf (agc::left m))))
	       ))))
      (find-leaf start))
    leaves))

;;; Get a single string of the spelling of the words in a tree.
(defun string-from-tree (mt)
  (agu:string-from-list (list-from-tree mt)))
