;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This is the interface to the Vosk/Kaldi speech recgnition system.
(in-package :ags)

(defvar *MINCONF* 40 )

;;; Process input from the Kaldi recognizer.
(defun listen-receive (msg np)
  (declare (ignore np))
  (log:warn "Vosk message ~a" msg))

;;; Create the network connection to the Vosk API.  Any incoming
;;; messages will go to the 'vreceive' function.
(defvar *jport*)
(defun vconnect ()
  (setq *jport* (agu:connect
		 "127.0.0.1" 10500
		 :handler 'vreceive
		 :name "Julius"))
  )

;; Send a command to Vosk.
(defun listen-control (cmd)
  (format T "~a~%" cmd))

(defun listen-stop ()
;;    (uiop:run-program "killall -q julius" :ignore-error-status T)
    (sleep 1)
  )

;;;; Start up Vosk, supplying its configuration file.
(defun listen-start ()
  (let ((path (asdf:system-relative-pathname :agador #p"data/")))
    (uiop:launch-program
     (format NIL "vosk-api ~a~a"
	     path
	     "toki-model")
     :output *standard-output*)
    ;; Give it time to start up before we connect to it.
    (sleep 2)
    (vconnect)
  ))

;;;; The "lexicon" needs to be in a special format for the
;;;; speech recognizer packages, along with phonetic information.
;;;; Here is where we generate that file.
(defun make-lexicon (filename)
  (let ((k-dict
	 (open
	  filename
	  :direction :output
	  :if-exists :supersede)))

    ;; The 'silence markers' are always there.
    (format k-dict "SENT-START sil~%")
    (format k-dict "SENT-END sil~%")
    (format k-dict "<OOV> UNK~%")

    ;; And write all the words to the Kaldi dictionary.
    ;; This will need to be sorted.
    (agp::all-words
     #'(lambda (spell say)
	 (format k-dict "~a ~a~%"
		 (string-upcase spell)
		 (string-upcase say))))
    
    (close k-dict)
))
