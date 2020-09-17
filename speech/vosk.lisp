;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

;;;; This is the interface to the Vosk/Kaldi speech recgnition system.
(in-package :ags)

(defvar *listener* NIL)
(defvar *enabled* T)

;;; Process input from the Kaldi recognizer.
(defun speech-listener (stream)
  (loop for line = (read-line stream nil)
     until (eq line NIL)
     do
       (when *enabled*
	 (format T "Heard: ~a~%" line))
       ))

;; Send a command to Vosk.
(defun listen-control (onoff)
  (setf *enabled* onoff))

(defun listen-stop ()
  (when *listener*
    (uiop:terminate-process *listener*))
  )

;;;; Start up the Vosk recognizer.  It will write recognized text
;;;; to its output stream which 'listen-receive' will interpret.
(defun listen-start ()
  (setf *listener*
    (uiop:launch-program
     (format NIL "vosk-listener ~a~a"
	     AGC::+data-directory+
	     "SpeechModel")
     :output :stream))

  ;; Start thread that reads the subprocess output
  (sb-thread:make-thread
	 'speech-listener
	 :name "VOSK"
	 :arguments (list
		     (uiop:process-info-output *listener*))))

;;;; The "lexicon" needs to be in a special format for the
;;;; speech recognizer and include phonetic information.
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
