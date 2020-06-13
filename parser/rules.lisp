;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGP)

;;;; A 'routing table' of grammatical functions, derived from the
;;;; rules. It is a two-dimensional hash-table, first keyed by
;;;; the function of a 'current' node and then keyed by the function
;;;; of a 'goal' node.  The value at the intersection can be:
;;;; 1) Missing or NIL => do not proceed down this branch.
;;;; 2) T => This is the goal node
;;;; 3) AGP:LEFT => Take the left downward path
;;;; 4) AGP:RIGHT => Take the right downward path.
(defvar *route* (make-hash-table :size 10))

;;; A hash table of rules, keyed on the right hand term.  The parser
;;; uses this to detect interesting adjacent terms.
(defvar *rules*)

(defun route-from (fn)
  (let ((ftable (gethash fn *route*)))
    (unless ftable
	(setf (gethash fn *route*) (make-hash-table :size 10)))
    (gethash fn *route*)))

(defun route-path (from toward)
  (let ((table (route-from from)))
     (gethash toward table)))

(defun add-route (startfn goal side)
  (let ((ftable (route-from startfn)))
    (unless (gethash goal ftable)
      (setf (gethash goal ftable) side))))

;;; Copy lower level routes up so we can find intermediate steps.
(defun merge-routes ()
  (loop for start being the hash-key
     using (hash-value start-table) of *route*
     do
       (loop for upper-goal being the hash-key
	  using (hash-value upper-path) of start-table
	  do
	    (let ((lower-table (gethash upper-goal *route*)))
	      (if lower-table
		  (loop for lower-goal being the hash-key of lower-table
		     do
		       (add-route start lower-goal upper-path)))))))

(defun print-routes ()
  "Print out the entire route table"
  (loop for s being the hash-key
     using (hash-value st) of *route*
     do
       (format T "~a to~%" s)
       (loop for g being the hash-key
	  using (hash-value p) of st
	  do
	    (format T "    ~a go ~a~%" g p))))

;;; Use the routing table to find specific parts of a sentence.
;;; Given an upper node and a desired grammatical Function,
;;; find a USAGE.
(defun word-at (start goal)
  (let ((probe start))
    (loop named searching do
	 (case (agc:seek probe goal)
	   ;; Can't get there form here
	   ((NIL) (return-from searching NIL))
	   ;; Go to left child
	   (AGC:LEFT (setf probe (agc:left probe)))
	   ;; Go to right child
	   (AGC:RIGHT (setf probe (agc:right probe)))
	   ;; This is the desired node.
	   ((T) (return-from searching probe))))))

;;; Each entry is a list of rules with the same right term.  We
;;; create the list the first time, and add to it thereafter.  When
;;; trying out potential rules, we always know what the right side
;;; is, so this speeds up the search.
(defun add-rule (lfn rfn rslt act)
  (let* (
	 (lfn1 (intern lfn :AGF))
	 (rfn1 (intern rfn :AGF))
	 (rslt1 (intern rslt :AGF))
	 (act1 (if act (intern act :AGA) NIL))
	 (newrule
	  (make-instance 'rule
			:left lfn1 :right rfn1
			:result rslt1 :action act1))
	 (oldrules (gethash rfn1 *rules*)))

    ;; Add new rule to the list of all with same right term
    (setf (gethash rfn1 *rules*)
	  (if oldrules (push newrule oldrules)
	      (list newrule)))

    ;; Remember paths downward through the rules.
    (add-route rslt1 lfn1 'AGC:LEFT)
    (add-route rslt1 rfn1 'AGC:RIGHT)))

;; Get a list of the rules with a specified right side term.
(defun rules-for (fn) (gethash fn *rules*))

(defparameter +rule+ "^(\\w+) (\\w+) (\\w+)\\s?(\\w+)?" )
(defparameter +cmnt+ "^\\s*#" )

;;; Load the rules at startup.
(defun load-rules (fname)
  "Load rules from a file"
  ;; Initialize rules and routes
  (setq *rules* (make-hash-table :size 20))
  (setq *route* (make-hash-table :size 10))

  ;; Load the basic three-term rules.
  (with-open-file (stream fname)
    (loop for line = (read-line stream NIL)
       until (eq line NIL) do
	 (if (ppcre:scan +cmnt+ line)
	     NIL
	     (ppcre:register-groups-bind
		 (lfn rfn rslt act)
		 (+rule+ (string-upcase line))
	       (add-rule lfn rfn rslt act)))))

  ;; Find the first step of multi-step routes
  (merge-routes))

(defun init-rules (lang)
  "Load rules from a file."
  (load-rules
   (format NIL "~a/~a.rules" AGC:+data-directory+ lang)))

(defun print-rules ()
  "Print all rules according to right-hand term."
  (loop for rfn being the hash-key
     using (hash-value llist) of *rules*
     do
       (format T "For ~a: " rfn)
       (dolist (r llist) (print-object r T))
       (terpri)))
       
