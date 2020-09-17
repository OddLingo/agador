;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

(defparameter +data-directory+
  (asdf:system-relative-pathname :agador #p"data"))

(opts:define-opts
    (:name :confidence
           :description "Minimum confidence level"
           :short #\m
           :long "minimum"
           :arg-parser #'parse-integer)
    (:name :count
           :description "Number of sentences to generate"
           :short #\n
           :long "count"
	   :default 10
           :arg-parser #'parse-integer)
    (:name :prompts
           :description "Name of prompt file to write"
           :short #\p
           :long "prompts"
           :arg-parser #'identity)
    (:name :lexicon
           :description "Name of lexicon file to write"
           :short #\l
           :long "lexicon"
           :arg-parser #'identity)
    (:name :corpus
           :description "Name of text corpus file to write"
           :short #\c
           :long "corpus"
           :arg-parser #'identity))

(defun get-options ()
  "Process command line options"
  (multiple-value-bind (options free-args)
      (opts:get-opts)
    (declare (ignore free-args))
    (let (
;;	  (conf (getf options :confidence))
	  (corpus (getf options :corpus))
	  (prompts (getf options :prompts))
	  (lexicon (getf options :lexicon))
	  (count (getf options :count))
	  )
      (when lexicon
	(ags::make-lexicon lexicon)
	(sb-ext:quit))
      (when corpus
	(agp::make-text count corpus T)
	(sb-ext:quit))
      (when prompts
	(agp::make-text count prompts NIL)
	(sb-ext:quit))
;;      (if conf (setq AGS:*MINCONF* conf))
      )
    ))

(defun run ()
  "Top level function called at startup."
  (declare (optimize (debug 3) (speed 0)))
  ;; Initial housekeeping
  (setf *random-state* (make-random-state t))

  ;; Process command line.
  (get-options)

  ;; Set up the standard logging package.
  (log:config
   :SANE :DAILY "agador.log" :BACKUP NIL
   :NOFILE)
  (log:info "Start")
  
  ;; Open the memory database
  (agm:db-open)

  ;; Start the parser thread.
  (agp:start-parser)

  ;; Start background tasks
;;  (agu:sked-later 5 #'aga:wx-tropical)
;;  (aga:start-security)
  
  ;; Start speech interfaces
  (ags:tstart)
;;  (ags:listen-start)

  (ags:say "mi kute")  ;; "I am listening."

  ;; Start the visual user interface with a condition handler
  ;; (handler-case
  ;;     (agm:explore)
  ;;   (error (e)
  ;;     (log:error e)))
  (agm:explore)
  
  ; Clean up to exit.
;;  (ags:listen-stop)
  (agm:db-close)
  (log:info "Finish")
  )
