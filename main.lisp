;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

(defvar *lang* "english")
(defparameter +data-directory+
  (asdf:system-relative-pathname :agador #p"data"))


(opts:define-opts
    (:name :language
           :description "Set natural language"
           :short #\l
           :long "lang")
    (:name :confidence
           :description "Minimum confidence level"
           :short #\c
           :long "conf"
           :arg-parser #'parse-integer))

;;(defun get-options ()
;  (multiple-value-bind (options free-args)
;    (opts:get-opts)
;
;    (let ((lang (getf options :lang))
;	  (conf (getf options :confidence))
;	  )
;      (if lang (setq *lang* lang))
;      (if conf (setq AGS:*MINCONF* conf))
;      )
;    ))
  
(defun run ()
;  (get-options)
  
  ;; Open the memory database
  (agm:db-open)

  ;; Load grammar analysis rules and start the parser thread.
  (agp:start-parser *lang*)

  ;; Start background tasks
  (agu:sked-later 5 #'aga:wx-tropical)
  (aga:start-security)
  
  ;; Start Julius speech recognizer
  (ags:tstart)
  (ags:jstart *lang*)

  (ags:say "I'm here.")

  ;; Start the visual user interface
  (agm:explore)

  ; Clean up to exit.
  (ags:jstop)
  (agm:db-close)
  )


