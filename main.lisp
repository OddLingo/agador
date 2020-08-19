;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

(defparameter +data-directory+
  (asdf:system-relative-pathname :agador #p"data"))


(opts:define-opts
    (:name :confidence
           :description "Minimum confidence level"
           :short #\c
           :long "conf"
           :arg-parser #'parse-integer))

;;(defun get-options ()
;  (multiple-value-bind (options free-args)
;    (opts:get-opts)
;
;    (let (
;	  (conf (getf options :confidence))
;	  )
;      (if conf (setq AGS:*MINCONF* conf))
;      )
;    ))
  
(defun run ()
;  (get-options)

  ;; Set up the standard logging package.
  (log:config
   :SANE :DAILY "agador.log" :BACKUP NIL
   :NOFILE)
  (log:info "Start")
  
  ;; Open the memory database
  (agm:db-open)

  ;; Load grammar analysis rules and start the parser thread.
  (agp:start-parser)

  ;; Start background tasks
  (agu:sked-later 5 #'aga:wx-tropical)
  (aga:start-security)
  
  ;; Start Julius speech recognizer
  (ags:tstart)
  (ags:jstart "toki")

  (ags:say "toki.")

  ;; Start the visual user interface
  (handler-case
      (agm:explore)
    (error (e)
      (log:error e)))

  ; Clean up to exit.
  (ags:jstop)
  (agm:db-close)
  (log:info "Finish")
  )


