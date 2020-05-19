;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :ags)

(defvar *MINCONF* 60 )
(defvar *word-classes* (make-array 20 :fill-pointer 0 :adjustable t ))

;;;; Regex patterns for the important Julius messages.
(defconstant +shypo+
  "\\s*<SHYPO RANK=\"(\\d+)\" SCORE=\"([0-9\\\.\\-]+)\" GRAM=\"(\\d+)\">" )

(defconstant +whypo+
  "\\s*<WHYPO WORD=\"(\\w+)\" CLASSID=\"(\\d+)\" PHONE=\"([a-z0-9 ]+)\" CM=\"([0-9\\\.\\-]+)\"/>" )

(defconstant +input+
  "<INPUT STATUS=\"(\\w+)\" TIME=\"(\\d+)\"/>" )

(defconstant +inparm+
  "<INPUTPARAM FRAMES=\"(\\d+)\" MSEC=\"(\\d+)\"/>" )

(defconstant +class-num+ "(\\d+)\\s+(\\w+)" )

(defclass jstate ()
  (
   (active :initform T :accessor active)
   (ready :initform NIL :accessor ready)
   (recognizing :initform NIL :accessor recog)
   (sent :initform NIL :accessor sent)
   (recstart :accessor starttime :initarg :starttime)
  ))

(defvar *jstate* (make-instance 'jstate))

(defclass jword () (
  (spell :initarg :spell :accessor spell)
  (class :initarg :class :accessor word-class :type fixnum)
  (phonemes :initarg :phonemes :accessor word-phonemes :type string)
  (confidence :initarg :cm :accessor word-cm :type float)
  ))

(defclass jsent () (
  (rank :initarg :rank :accessor sent-rank :type float)
  (score :initarg :score :accessor sent-score :type float)
  (gram :initarg :gram :accessor sent-gram :type fixnum)
  (words :initform (make-array 10 :fill-pointer 0 :adjustable t )
	 :accessor sent-words :type list)
  ))

;; Extract list of consed spelling and class names and send
;; it to the tree parser.  We leave out the silence markers at
;; the beginning and end.
(defmethod words-to-parser ((s jsent))
  (let* ((wordlist NIL))
    (loop for w across (sent-words s) do
	 (let ((fn (elt *word-classes* (word-class w))))
	   (if (not (or (string= fn "NS_B")
			(string= fn "NS_E")))
	       (push (cons (spell w) fn) wordlist)
	       )
	   )
	 )
    (agp::parse-msg (reverse wordlist))
    (agu:term "Sentence score ~a~%"
	      (floor (sent-score s)))
    )
  )

(defmethod addword (w (s jsent))
  (vector-push-extend w (sent-words s))
  )

(defmethod avgconfidence ((s jsent))
  (let ((total 0))
    (loop for w across (sent-words s)
       do (setf total (+ total (word-cm w))))
    (floor (/ total (length (sent-words s))))
    ))

(defmethod minconfidence ((s jsent))
  (let ((minimum 1.0))
    (loop for w across (sent-words s) do
	 (if (< (word-cm w) minimum) (setf minimum (word-cm w)))
	 )
    minimum
    ))

;; Load the class-number-to-name table.  This was generated by
;; the mkdfa utility.
(defun load-classes (langmodel)
  "Load word classes"
  (with-open-file (terms (format NIL "~a.term" langmodel))
    (loop for line = (read-line terms NIL)
       while line do
	 (let ((class (cl-utilities:split-sequence
		       '#\Tab
		       (string-trim " " line)))
	       )
	   ;; We internalize the class names in the :AGF namespace
	   ;; because that is where the grammar rules will look
	   ;; for them.
	   (vector-push-extend
	    (intern (cadr class) :agf)
	    *word-classes*)
	   )
	 )
    )
;  (format T "Classes ~a~%" *word-classes*)
  )

;; Match an INPUT report
(defun matched-input (jtxt)
  (ppcre:register-groups-bind
   (state ('parse-integer stime))
   (+input+ jtxt :sharedp T)
   (progn
     (cond
       ((equal state "LISTEN")  (setf (ready *jstate*) T))
       ((equal state "STARTREC") (setf (starttime *jstate*) stime))
       ((equal state "ENDREC") T)
       ;; (agu:term "  took ~d~%" (- stime (starttime *jstate*)))
       )
     T)
     )
    )

(defun matched-param (jtxt)
  (ppcre:register-groups-bind
   (('parse-integer frames)
    ('parse-integer msec))
   (+inparm+ jtxt :sharedp T)
   (progn
     (setf (ready *jstate*) NIL)
     (agu:term "  ~a frames took ~a ms~%" frames msec)
     T)
    ))
  
;; Match a <SHYPO that is the start of a sentence report.
(defun matched-sent (jtxt)
  (ppcre:register-groups-bind
   (('parse-integer srank)
    ('read-from-string sscore)
    ('parse-integer sgram))
   (+shypo+ jtxt :sharedp T)
   (make-instance 'jsent
		  :rank srank
		  :score sscore
		  :gram sgram)
    ))

;; Match a <WHYPO that is a word report within a sentence.
(defun matched-word (txt)
  (ppcre:register-groups-bind
   (wspell ('parse-integer wclass) wph ('read-from-string wcm))
   (+whypo+ txt :sharedp T)
   (make-instance 'jword
		  :spell wspell :class wclass
		  :phonemes wph :cm wcm)
   ))

;; Done receiving a complete sentence.  If we are confident
;; enough in the recognition, we send it to deep grammar analysis.
(defun analyze ()
  (let* ((s (sent *jstate*))
	 (mc (floor (* 100 (minconfidence s)))))
    (if (> mc *minconf*)
	(progn
	  (agu:set-status "Confidence ~a~%" mc)
	  (words-to-parser s)
	  )
	(agu:set-status "Only ~a% confidence~%" mc)
	)
    )
  )

;; Process messages from Julius.
(defun jreceive (msg np)
  (declare (ignore np))
  (let ((m))
;  (agu:term "J> |~a|~%" msg)
  (cond
    ; Ignore the dots
    ((equal "." msg) T)

    ; During recognition reports
    ((setf m (matched-word msg))
     (progn
       (agu:term "  Word ~a at ~a~%" (spell m) (word-cm m))
       (addword m (sent *jstate*))))

    ((setf m (matched-sent msg))
       (setf (sent *jstate*) m))

    ((matched-param msg) T)

    ((search "</SHYPO>" msg)
     (progn
       (setf (recog *jstate*) NIL)
       (analyze)))

     ; End of recognition output
    ((search "</RECOGOUT>" msg)
     (setf (recog *jstate*) NIL))

    ; Start recognition output
    ((equal msg "<RECOGOUT>")
     (setf (recog *jstate*) T))
    ((equal msg "<STARTRECOG/>") T)    
    ((equal msg "<ENDRECOG/>") T)
    ((equal msg "<RECOGFAIL/>") (agu:set-status "Could not recognize"))
    ((equal msg "<STARTPROC/>") (setf (active *jstate*) T))
    ((equal msg "<STOPPROC/>") (setf (active *jstate*) NIL))

    ; Input state
    ((matched-input msg) T)

    ; Everything else
    (T (agu:term "Unhandled Julius ~a~%" msg))
    ))
  )

(defvar *jport*)
(defun jconnect ()
  (setq *jport* (agu:connect
		 "127.0.0.1" 10500
		 :handler 'jreceive
		 :name "Julius"))
  )

;; Send a command to Julius.
(defun jsend (cmd)
  (agu:send *jport* (format NIL "~a~%" cmd)))

(defun jstop ()
    (uiop:run-program "killall -q julius" :ignore-error-status T)
    (sleep 1)
  )

;;;; Start up Julius, supplying its configuration file.  We also load
;;;; the "term" file that maps the word class numbers to their names.
(defun jstart (confname)
  (let ((path (asdf:system-relative-pathname :agador #p"data/")))
    (load-classes (format NIL "~a~a" path confname))
    (jstop)
    (uiop:launch-program
     (format NIL "julius -C ~a~a.jconf"
	   path
	   confname)
     :output *standard-output*)
    (sleep 2)
    (jconnect)
  ))

