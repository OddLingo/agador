;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;; A time-driven event scheduler.  Scheduled function calls run
;;;; in the scheduler thread, which is created and destroyed
;;;; as required.

(in-package :AGC)

;; Sorted list of (time . function) pairs.
(defvar *schedule* NIL)
(defvar *sked-thread* NIL)
(defvar *sked-mutex* (sb-thread:make-mutex))

;; Comparison function for sorting the event list.
(defun sked-less (x y) (< (car x) (car y)))

;; The main loop of the scheduler thread.  It sleeps waiting for
;; the earliest schedule event.
(defun sked-loop ()
  (loop
     (if *schedule*
	 (let* ((nxt (first *schedule*))
		(togo (- (car nxt) (get-universal-time)))
		)
	   (if (> togo 0) (sleep togo))
	   (funcall (cdr nxt))
	   ;; Remove this event from the list and re-sort.
	   (sb-thread:grab-mutex *sked-mutex*)
	   (setq *schedule* (remove nxt *schedule*))
	   (sort *schedule* #'sked-less)
	   (sb-thread:release-mutex *sked-mutex*)
	   )

	 ;; Nothing scheduled - exit the thread.
	 (return)
	 )
     )
  )

;; Called by other code to schedule future events.  The first
;; parameter is a local-time:timestamp.
(defun sked-add (tm action)
  (sb-thread:grab-mutex *sked-mutex*)
  (push (cons
	 (local-time:timestamp-to-universal tm)
	 action)
	*schedule*)
  (sb-thread:release-mutex *sked-mutex*)

  ;; If we are called from a scheduled function, we let the loop
  ;; deal with it.  Otherwise we interrupt the existing loop if
  ;; it exists, and start the loop thread if not.
  (if (not (equal sb-thread:*current-thread* *sked-thread*))
      (if (null *sked-thread*)
	  (setq *sked-thread*
		(sb-thread:make-thread 'sked-loop :name "Scheduler"))
	  (sb-thread:interrupt-thread *sked-thread* #'sked-loop)
      )
  )
)

;; Similar to sked-add except that the time is in integer seconds
;; from now.
(defun sked-later (secs action)
  (sked-add
   (local-time:timestamp+
    (local-time:now) secs :sec)
   action ))

(defun test ()
  (format T "tick~%" )
  (sked-later 4 #'test)
  )
