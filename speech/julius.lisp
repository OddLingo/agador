;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :ags)

(defvar *MINCONF* 40 )
;;;; Regex patterns for the important Julius messages.
(defparameter +shypo+
  "\\s*<SHYPO RANK=\"(\\d+)\" SCORE=\"([0-9\\\.\\-]+)\" GRAM=\"(\\d+)\">" )

(defparameter +whypo+
  "\\s*<WHYPO WORD=\"(\\w+)\" CLASSID=\"\\d+\" PHONE=\"[a-z0-9 ]+\" CM=\"([0-9\\\.\\-]+)\"/>" )

(defparameter +input+
  "<INPUT STATUS=\"(\\w+)\" TIME=\"(\\d+)\"/>" )

(defparameter +class-num+ "(\\d+)\\s+(\\w+)" )

(defclass jstate ()
  (
   (active :initform T :accessor active)
   (ready :initform NIL :accessor ready)
   (recognizing :initform NIL :accessor recog)
   (sent :initform NIL :accessor sent)
   (recstart :accessor starttime :initarg :starttime)
  ))

(defvar *jstate* (make-instance 'jstate))

;;; One word as reported by Julius
(defclass jword () (
  (spell :initarg :spell :accessor spell :type string)
  (confidence :initarg :cm :accessor word-cm :type float)
  ))

;; One sentence as reported by Julius.
(defclass jsent () (
  (rank :initarg :rank :accessor sent-rank :type float)
  (score :initarg :score :accessor sent-score :type float)
  (gram :initarg :gram :accessor sent-gram :type fixnum)
  (words :initform (make-array 20
			       :fill-pointer 0
			       :adjustable t )
	 :accessor sent-words :type vector)
  ))

;;; Add a Word to a Sentence.
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
  "Compute the minimum confidence in a sentence"
  (let ((minimum 100))
    (loop for w across (sent-words s) do
	 (if (< (word-cm w) minimum) (setf minimum (word-cm w))))
    minimum
    ))

;; Extract list of word spellings and send it to the tree parser.
(defmethod words-to-parser ((s jsent))
  (let* ((wordlist NIL))
    (loop for w across (sent-words s) do
	 (push (spell w) wordlist))
    (agp::parse (nreverse wordlist))
    (log:info "Sentence score ~a~%"
	      (floor (sent-score s)))))

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
     T)))

;;; Match a <SHYPO that is the start of a sentence report.
(defun matched-sent (jtxt)
  (ppcre:register-groups-bind
   (('parse-integer srank)
    ('read-from-string sscore)
    ('parse-integer sgram))
   (+shypo+ jtxt :sharedp T)
   (make-instance 'jsent
		  :rank srank
		  :score sscore
		  :gram sgram)))

;;; Match a <WHYPO that is a word report within a sentence.
;;; All we care about is the spelling and the confidence.
(defun matched-word (txt)
  (ppcre:register-groups-bind
   (wspell ('read-from-string wcm))
   (+whypo+ txt :sharedp T)
   (make-instance 'jword
		  :spell wspell
		  :cm (floor (* 100.0 wcm)))))

;;; Done receiving a complete sentence.  If we are confident
;;; enough in the recognition, we send it to deep grammar analysis.
(defun analyze ()
  (let* ((s (sent *jstate*))
	 (mc (minconfidence s)))
    (if (> mc *minconf*)
	(progn
	  (agu:set-status "Confidence ~d~%" mc)
	  (words-to-parser s))
	(agu:set-status "Only ~d% confidence~%" mc))))

;;; Process messages from Julius "module mode".
(defun jreceive (msg np)
  (declare (ignore np))
  (let ((m))
    (cond
    ;; Ignore the dots
    ((equal "." msg) T)

    ;; Recognized a word.  Ignore the utterance start and end markers.
    ((setf m (matched-word msg))
     (let ((sp (spell m)))
       (cond
	 ((equal sp "s") T)
	 ((equal sp "es") T)
	 (T (agu:term "  ~a ~d%~%" (spell m) (word-cm m))
	    (addword m (sent *jstate*))))))

    ;; Recognized a sentence start.
    ((setf m (matched-sent msg)) (setf (sent *jstate*) m))

    ;; End of info about a sentence.
    ((search "</SHYPO>" msg)
     (progn
       (setf (recog *jstate*) NIL)
       (analyze)))

     ; End of recognition output
    ((search "</RECOGOUT>" msg) (setf (recog *jstate*) NIL))

    ; Start recognition output
    ((search "<INPUTPARAM" msg) (setf (ready *jstate*) NIL))
    ((equal msg "<RECOGOUT>")
     (setf (recog *jstate*) T)
     (agu:term "-----~%"))
    ((equal msg "<STARTRECOG/>") T)    
    ((equal msg "<ENDRECOG/>") T)
    ((equal msg "<RECOGFAIL/>") (agu:set-status "Could not recognize"))
    ((equal msg "<STARTPROC/>") (setf (active *jstate*) T))
    ((equal msg "<STOPPROC/>") (setf (active *jstate*) NIL))

    ; Input state
    ((matched-input msg) T)

    ; Everything else
    (T (log:warn "Julius message ~a" msg))
    ))
  )

;;; Create the network connection to the Julius program.  Any incoming
;;; messages will go to the 'jreceive' function.
(defvar *jport*)
(defun jconnect ()
  (setq *jport* (agu:connect
		 "127.0.0.1" 10500
		 :handler 'jreceive
		 :name "Julius"))
  )

;; Send a command to Julius.
(defun listen-control (cmd)
  (agu:send *jport* (format NIL "~a~%" cmd)))

(defun listen-stop ()
    (uiop:run-program "killall -q julius" :ignore-error-status T)
    (sleep 1)
  )

;;;; Start up Julius, supplying its configuration file.
(defun listen-start ()
  (let ((path (asdf:system-relative-pathname :agador #p"data/")))
    (listen-stop)
    (uiop:launch-program
     (format NIL "julius -C ~a~a.jconf"
	     path
	     "toki.jconf")
     :output *standard-output*)
    ;; Give it time to start up before we connect to it.
    (sleep 2)
    (jconnect)
  ))

;;;; The dictionary needs to be in a special format for the
;;;; speech recognizer packages, along with phonetic information.
;;;; Here is where we generate that file.
(defun jgenerate ()
  (let ((funs (words-by-fn))
	(voca
	 (open
	  (format NIL "~a/toki.voca" AGC:+data-directory+)
	  :direction :output
	  :if-exists :supersede)))
    (labels
	((print-word (file word)
	   "Write vocabulary for other programs"
	   (format file "~a~C~a~%" word '#\Tab (phonetics word)))
	 (fn-to-voca (fn)
	   (format voca "~%% ~a~%" fn)
	   (dolist (word (gethash fn funs))
	     (print-word voca word))))

      ;; The 'silence markers' are always there.
      (format voca "# This is a generated file.  Do not edit.~%")
      (format voca "% NS_B~%s	[]  sil~%")
      (format voca "% NS_E~%es	[]  sil~%")

      ;; Now write all the words to the voca file, grouped by function,
      ;; generating phonetics along the way.
      (dolist (fn (alexandria:hash-table-keys funs))
	(fn-to-voca fn))
      )
    
    (close voca)
))
