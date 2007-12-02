;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; events.lisp --- io.multiplexer test suite.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:iolib-tests)

(defmacro with-event-base/for-each-mux ((base &rest initargs) &body body)
  `(let ((failed-list))
     (dolist (mux (mapcar #'cdr *available-multiplexers*) failed-list)
       (handler-case
           (with-event-base (,base :mux mux ,@initargs)
             ,@body)
         (error (err)
           (push (cons mux err) failed-list))))))

(deftest timeout.1
    (with-event-base/for-each-mux (base)
      (event-dispatch base :timeout 0))
  nil)

(deftest timeout.2
    (with-event-base/for-each-mux (base)
      (let ((cb nil))
        (add-timeout base (lambda (fd event)
                            (declare (ignore fd))
                            (setq cb event))
                     30)
        (event-dispatch base :timeout 0)
        (assert (null cb))))
  nil)

(deftest timeout.3
    (with-event-base/for-each-mux (base)
      (let ((cb nil))
        (add-timeout base (lambda (fd event)
                            (declare (ignore fd))
                            (setq cb event))
                     0)
        (event-dispatch base :one-shot t)
        (assert (eq cb :timeout))))
  nil)

;;; regression test: timeouts' absolute times used used to be
;;; incremented with the relative time ad infinitum.
(deftest timeout.4
    (with-event-base/for-each-mux (base)
      (let ((cb nil))
        (add-timeout base (lambda (fd event)
                            (declare (ignore fd))
                            (setq cb event))
                     1.5)
        (event-dispatch base :one-shot t :timeout 2)
        (assert (eq cb :timeout))))
  nil)

(defun timeout-cb (fd event)
  (declare (ignore fd event))
  (error "timeout"))

(defmacro waiting-for-event ((base fd event-type) &body body)
  (with-unique-names (fd-arg event-arg)
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
(deftest event-base-with-sockets
    (with-event-base (base)
      (with-socket (passive :family :ipv4 :connect :passive
                            :local-host +ipv4-unspecified+)
        (with-socket (active :family :ipv4 :remote-port (local-port passive)
                             :remote-host #(127 0 0 1))
          (add-timeout base #'timeout-cb 5)
          (let (peer)
            (waiting-for-event (base (fd-of passive) :read)
              (setq peer (accept-connection passive)))
            (assert (socket-open-p peer)))
          ;; TODO: send and receive some stuff
          ))
      nil)
  nil)
