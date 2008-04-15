;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Miscellaneous utilities.
;;;

(in-package :io.multiplex)

(defun timeout->timeval (timeout tv)
  (with-foreign-slots ((sec usec) tv timeval)
    (multiple-value-bind (%sec %usec) (decode-timeout timeout)
     (setf sec  %sec
           usec %usec))))

(defun timeout->timespec (timeout ts)
  (with-foreign-slots ((sec nsec) ts timespec)
    (multiple-value-bind (%sec %usec) (decode-timeout timeout)
      (setf sec  %sec
            nsec (* 1000 %usec)))))

(defun timeout->milisec (timeout)
  (if timeout
      (multiple-value-bind (sec usec) (decode-timeout timeout)
        (+ (* sec 1000)
           (truncate usec 1000)))
      -1))

(defmacro flags-case (mask &body clauses)
  (once-only (mask)
    `(progn ,@(loop :for clause :in clauses
                 :collect `(when (logtest ,(let ((flags (first clause)))
                                                (if (listp flags)
                                                    `(logior ,@flags)
                                                    flags))
                                          ,mask)
                             ,(second clause))))))

(defmacro ignore-and-print-errors (&body body)
  `(handler-case (locally ,@body)
     (error (error)
       (warn "Caught a ~A: ~A, ignoring it."
             (type-of error) error))))
