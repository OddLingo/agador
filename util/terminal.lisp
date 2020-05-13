;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGU)

(defun gfun-from-string (s) (intern s :AGF))

(defun words-from-string (s)
  (split-sequence '#\Space (string-trim " " s)))
  
;; The inverse of words-from-string.
(defun string-from-list (l) (format NIL "~{~a~^ ~}" l))

;; Read one line from a file and split it into words.
;; Returns NIL at end-of-file.
(defun words-from-file (fi)
  (let ((line (read-line fi nil)))
    (if (null line) nil
	(words-from-string line))
    )
  )

(defvar *tmtx* (sb-thread:make-mutex :name "Terminal"))
(defun use-term () (sb-thread:grab-mutex *tmtx*))
(defun release-term () (sb-thread:release-mutex *tmtx*))

;; Console manipulation with escape sequences.
(defun clear ()
  (format T "~C[H~C[J" (code-char 27) (code-char 27))
  )
(defun setxy (x y)
  (format T "~C[~d;~dH" (code-char 27) y x)
  )
(defun set-color (fg bg)
  (format T "~C[~d;~dm"
	  (code-char 27)
	  (+ 30 fg)
	  (+ 40 bg)))

;; Use this in place of direct calls to FORMAT
(defun term (fmt &rest args)
  (use-term)
  (apply 'format T fmt args)
  (release-term)
  )
