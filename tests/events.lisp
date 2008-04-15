;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- io.multiplexer test suite.
;;;

(in-package :iolib-tests)

(in-suite* :io.multiplex :in :iolib)

(defmacro with-event-base/for-each-mux ((base &rest initargs) &body body)
  `(let ((failed-list))
     (dolist (mux (mapcar #'cdr *available-multiplexers*) failed-list)
       (handler-case
           (with-event-base (,base :mux mux ,@initargs)
             ,@body)
         (error (err)
           (push (cons mux err) failed-list))))))

(test timeout.1
  (is-false
   (with-event-base/for-each-mux (base)
     (event-dispatch base :timeout 0))))

(test timeout.2
  (is-false
   (with-event-base/for-each-mux (base)
     (let ((cb nil))
       (add-timer base (lambda () (setq cb :timeout)) 30)
       (event-dispatch base :timeout 0)
       (assert (null cb))))))

(test timeout.3
  (is-false
   (with-event-base/for-each-mux (base)
     (let ((cb nil))
       (add-timer base (lambda () (setq cb :timeout)) 0)
       (event-dispatch base :one-shot t)
       (assert (eq cb :timeout))))))

;;; regression test: timeouts' absolute times used used to be
;;; incremented with the relative time ad infinitum.
(test timeout.4
  (is-false
   (with-event-base/for-each-mux (base)
     (let ((cb nil))
       (add-timer base (lambda () (setq cb :timeout)) 1.5)
       (event-dispatch base :one-shot t :timeout 2)
       (assert (eq cb :timeout))))))

(defun timeout-cb (fd event)
  (declare (ignore fd event))
  (error "timeout"))

(defmacro waiting-for-event ((base fd event-type) &body body)
  (with-gensyms (fd-arg event-arg)
    (once-only (base)
      `(progn
         (add-fd ,base ,fd ,event-type
                 (lambda (,fd-arg ,event-arg)
                   (when (eq ,event-arg :error)
                     (error "error with ~A" ,fd-arg))
                   ,@body)
                 :one-shot t)
         (event-dispatch ,base :one-shot t)))))

;;; FIXME: doesn't work with SELECT.
;;;        where ? it works here, on Linux. SIONESCU 2007.12.02
(test event-base-with-open-sockets
  (is-false
   (with-event-base (base)
     (with-open-socket (passive :family :ipv4 :connect :passive
                                :local-host +ipv4-unspecified+)
       (with-open-socket (active :family :ipv4
                                 :remote-port (local-port passive)
                                 :remote-host #(127 0 0 1))
         (add-timer base #'timeout-cb 5)
         (let (peer)
           (waiting-for-event (base (fd-of passive) :read)
                              (setq peer (accept-connection passive)))
           (assert (socket-open-p peer)))
         ;; TODO: send and receive some stuff
         ))
     nil)))
