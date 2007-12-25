;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; scheduler.lisp --- Controlling the queue of scheduled events
;;;                    and running expired timers.
;;;
;;; Copyright (C) 2003 Zach Beane <xach@xach.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge,publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :io.multiplex)

;;;
;;; Public interface
;;;

(defun schedule-timer (schedule timer)
  (priority-queue-insert schedule timer)
  (values timer))

(defun unschedule-timer (schedule timer)
  (priority-queue-remove schedule timer)
  (values timer))

(defun reschedule-timer (schedule timer)
  (incf (%timer-expire-time timer) (%timer-relative-time timer))
  (priority-queue-insert schedule timer))


;;;
;;; The scheduler
;;;

(defun peek-schedule (schedule)
  (priority-queue-minimum schedule))

(defun time-to-next-timer (schedule)
  (let ((timer (peek-schedule schedule)))
    (and timer (%timer-expire-time timer))))


;;;
;;; Expiring timers
;;;

(defun expire-timer (schedule timer)
  (symbol-macrolet ((function (%timer-function timer))
                    (relative-time (%timer-relative-time timer))
                    (one-shot (%timer-one-shot timer)))
    (if (%timer-new-thread-p timer)
        (bt:make-thread function :name "Auxiliary timer thread.")
        (funcall function))
    (when (and relative-time (not one-shot))
      (reschedule-timer schedule timer))))

(defun expire-pending-timers (schedule now)
  (let ((expired-p nil))
    (loop
       (let ((next-timer (peek-schedule schedule)))
         (unless next-timer
           (return-from expire-pending-timers))
         (cond ((timer-expired-p next-timer now)
                (setf expired-p t)
                (expire-timer schedule (priority-queue-extract-minimum schedule)))
               (t 
                (return-from expire-pending-timers expired-p)))))))
