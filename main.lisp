;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGC)

(defvar *lang* "english")

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

  ;; Start Julius speech recognizer
  (ags:tstart)
  (ags:jstart *lang*)

  (ags:say "I'm here.")

  ;; Start background tasks
  (agu:sked-later 8 #'aga:wx-tropical)
  
  ;; Start the visual user interface
  (agm:explore)

  ; Clean up to exit.
  (ags:jstop)
  (agm:db-close)
  )


