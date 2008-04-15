;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various time-related functions.
;;;

(in-package :io.multiplex)

;;;; Timeouts

(deftype timeout ()
  'double-float)

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (assert (or (not timeout)
              (and (typep timeout 'real)
                   (not (minusp timeout))))
          (timeout)
          "The timeout must be a non-negative real or NIL: ~S" timeout)
  (typecase timeout
    (null nil)
    (integer (values timeout 0))
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'timeout))
       (declare (type unsigned-byte q)
                (type timeout r))
       (values q (the (values unsigned-byte t) (truncate (* r 1d6))))))))

(defun normalize-timeout (timeout)
  (assert (and (typep timeout 'real)
               (not (minusp timeout)))
          (timeout)
          "The timeout must be non-negative: ~A" timeout)
  (coerce timeout 'timeout))

(defun abs-timeout (timeout)
  (+ (osicat:get-monotonic-time) (normalize-timeout timeout)))

(defun min-timeout (&rest timeouts)
  (collect-min (choose-if #'identity (scan timeouts))))
