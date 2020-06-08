;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :AGU)

(defun gfun-from-string (s) (intern s :AGF))

(defun words-from-string (s)
  (split-sequence '#\Space (string-trim " " s)))
  
;;; The inverse of words-from-string.
(defun string-from-list (l) (format NIL "~{~a~^ ~}" l))

;;; Read one line from a file and split it into words.
;;; Returns NIL at end-of-file.
(defun words-from-file (file)
  (let ((line (read-line file NIL)))
    (if (null line) NIL
	(words-from-string line))))

;;; A mutex prevents threads from mixing output lines.
(defvar *tmtx* (sb-thread:make-mutex :name "Terminal"))
(defun use-term () (sb-thread:grab-mutex *tmtx*))
(defun release-term () (sb-thread:release-mutex *tmtx*))

;;; Console manipulation with escape sequences.
(defun clear ()
  (format T "~C[H~C[J" (code-char 27) (code-char 27)))
(defun clear-eol ()
  (format T "~C[K" (code-char 27)))
(defun setxy (x y)
  (format T "~C[~d;~dH" (code-char 27) y x))
(defun set-color (fg bg)
  (format T "~C[~d;~dm"
	  (code-char 27)
	  (+ 30 fg)
	  (+ 40 bg)))

;;; Line numbers of various screen regions.
(defparameter +w1top+ 1)
(defparameter +w1bot+ 10)
(defparameter +sline+ 11)   ;; Status line
(defparameter +rtop+ 12)    ;; Top of scrolling region
(defparameter +rbot+ 37)    ;; Bottom of scrolling region

(defun set-scroll (&optional (yes T))
  "Enable/disable scrolling regions"
  (if yes
      (format T "~C[~d;~dr" (code-char 27) +rtop+ +rbot+)
      (format T "~Cr" (code-char 27))))

(defun clearw1 ()
  "Clear just Window 1"
  (set-color 7 0)
  (loop for row from +w1top+ to +w1bot+ do
       (setxy 1 row)
       (clear-eol)))

(defun set-status (fmt &rest args)
  "Replace text on the status line"
  (use-term)
  (setxy 1 +sline+)
  (set-color 0 6)
  (clear-eol)
  (apply 'format *standard-output* fmt args)
  (set-color 7 0)
  (force-output *standard-output*)
  (release-term))
 
;;; Use this in place of direct calls to FORMAT for simple logging.
(defun term (fmt &rest args)
  "Write text to the scrolling region"
  (use-term)
  (setxy 1 +rbot+)
  (apply 'format *standard-output* fmt args)
  (force-output *standard-output*)
  (release-term))
