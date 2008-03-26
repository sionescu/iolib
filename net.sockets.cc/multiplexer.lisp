;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: NIL -*-
;;;
;;; Copyright (C) 2006-2008, Attila Lendvai  <attila.lendvai@gmail.com>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :net.sockets.cc)

;; TODO why does this code have to wrap the fd in an fd-entry struct?

(defconstant +event-polling-timeout-in-seconds+ 2)

(defmacro with-lock-held-on-connection-multiplexer (multiplexer &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock-of ,multiplexer))
     ,@body))

(defun make-connection-multiplexer (name &key (fd-multiplexer-type 'epoll-multiplexer)
                                    (worker-count 1) (external-format :default))
  (make-instance 'connection-multiplexer
                 :fd-multiplexer-type fd-multiplexer-type
                 :worker-count worker-count
                 :name name
                 :external-format external-format))

(defmethod startup-connection-multiplexer ((self connection-multiplexer) &key)
  (with-lock-held-on-connection-multiplexer self
    (bind ((fd-multiplexer (fd-multiplexer-of self)))
      (when fd-multiplexer
        (error "connection-multiplexer ~A is already started" self))
      (setf fd-multiplexer (make-instance (fd-multiplexer-type-of self)))
      (setf (fd-multiplexer-of self) fd-multiplexer)
      (bind ((done nil))
        ;; TODO use alexa:unwi-protect-case
        (unwind-protect
             (progn
               (loop
                  :repeat (worker-count-of self)
                  :do (spawn-connection-multiplexer-worker-thread self))
               (setf done t))
          (unless done
            (io.multiplex::close-multiplexer fd-multiplexer)
            (setf (fd-multiplexer-of self) nil))))))
  self)

(defun spawn-connection-multiplexer-worker-thread (multiplexer)
  (bind ((worker-thread nil))
    ;; this here has a (theoretical) race condition, because MAKE-THREAD
    ;; also starts the thread right away.
    (setf worker-thread
          (bind ((standard-output *standard-output*)
                 (error-output *error-output*)
                 (debug-io *debug-io*))
            (bordeaux-threads:make-thread
             (lambda ()
               ;; TODO this rebinding is not right here. does it work at all?
               (bind ((*standard-output* standard-output)
                      (*error-output* error-output)
                      (*debug-io* debug-io)
                      (done nil))
                 (unwind-protect
                      (progn
                        (connection-multiplexer-worker-loop multiplexer)
                        (setf done t))
                   (deletef (workers-of multiplexer) worker-thread)
                   (unless done
                     ;; TODO proper error handling and debugging helpers
                     (warn "A connection-multiplexer worker thread died unexpectedly")))))
             :name (format nil "~A worker ~A"
                           (name-of multiplexer)
                           (length (workers-of multiplexer))))))
    (vector-push-extend worker-thread (workers-of multiplexer))))

(defun connection-multiplexer-worker-loop (multiplexer)
  (loop
     :named looping
     :do (progn
           (when (shutdown-requested-p multiplexer)
             (return-from looping))
           (process-some-connection-multiplexer-events multiplexer))))

(defmethod shutdown-connection-multiplexer ((self connection-multiplexer) &key force)
  (declare (ignore force))
  ;; TODO force thread exit?
  (setf (shutdown-requested-p self) t)
  (loop
     ;; instead of this loop we could use a conditional variable, but
     ;; probably it's not worth the trouble...
     :named looping
     :do (with-lock-held-on-connection-multiplexer self
           (when (zerop (length (workers-of self)))
             (return-from looping)))
     (sleep 1))
  (loop
     :with fd->connection = (fd->connection-of self)
     :for idx :from 0 :below (length fd->connection)
     :do (unless (null (aref fd->connection idx))
           (warn "A connection is still registered for fd ~A while shutting down ~A" idx self)))
  (io.multiplex::close-multiplexer (fd-multiplexer-of self))
  (setf (fd-multiplexer-of self) nil))

(defmethod register-connection ((multiplexer connection-multiplexer) (connection connection-with-continuation-mixin))
  (assert (continuation-of connection))
  (assert (wait-reason-of connection))
  (with-lock-held-on-connection-multiplexer multiplexer
    (bind ((fd (fd-of connection))
           (fd->connection (fd->connection-of multiplexer))
           (mux (fd-multiplexer-of multiplexer))
           (wait-reason (wait-reason-of connection)))
      (assert (and fd (not (zerop fd))))
      (assert mux)
      (when (>= fd (length fd->connection))
        (setf fd->connection
              (adjust-array fd->connection (max (* 2 (length fd->connection))
                                                fd)))
        (setf (fd->connection-of multiplexer) fd->connection))
      (assert (null (aref fd->connection fd)) () "There's already a registered connection on fd ~A!" fd)
      (setf (aref fd->connection fd) connection)
      (monitor-fd mux (aprog1
                          (make-fd-entry fd)
                        (setf (iomux::fd-entry-read-event it) (eq wait-reason :read))
                        (setf (iomux::fd-entry-write-event it) (eq wait-reason :write))))))
  (values))

(defmethod unregister-connection ((multiplexer connection-multiplexer) (connection connection-with-continuation-mixin))
  (with-lock-held-on-connection-multiplexer multiplexer
    (bind ((fd (fd-of connection))
           (fd->connection (fd->connection-of multiplexer))
           (mux (fd-multiplexer-of multiplexer)))
      (assert (and fd (not (zerop fd))))
      (assert (not (null (aref fd->connection fd))) () "There's no connection registered on fd ~A!" fd)
      (setf (aref fd->connection fd) nil)
      (unmonitor-fd mux (make-fd-entry fd))))
  (values))

(defmethod connection-registered-p ((multiplexer connection-multiplexer) (connection connection-with-continuation-mixin))
  (with-lock-held-on-connection-multiplexer multiplexer
    (bind ((fd (fd-of connection))
           (fd->connection (fd->connection-of multiplexer)))
      (assert (and fd (not (zerop fd))))
      (and (not (null (aref fd->connection fd)))
           (eq (aref fd->connection fd) connection)))))

(defun continue-connection (multiplexer connection)
  (assert (not (connection-registered-p multiplexer connection)))
  (bind (((:values result wait-reason)
          (funcall (continuation-of connection))))
    (if (eq result 'finished)
        (progn
          ;;(format *debug-io* "finished from CONTINUE-CONNECTION, closing connection ~A~%" connection)
          (assert (not (connection-registered-p multiplexer connection)))
          (close connection))
        (progn
          (setf (continuation-of connection) result)
          (setf (wait-reason-of connection) wait-reason)
          (with-lock-held-on-connection-multiplexer multiplexer
            (register-connection multiplexer connection))))))

(defmethod harvest-events ((connection-multiplexer connection-multiplexer) timeout)
  (harvest-events (fd-multiplexer-of connection-multiplexer) timeout))

(defun process-some-connection-multiplexer-events (multiplexer)
  (bind ((events (harvest-events multiplexer +event-polling-timeout-in-seconds+)))
    (loop
       :for entry :in events
       :do (bind (((fd event-types) entry)
                  (connection-ready-to-continue nil))
             (format t "fd: ~A, event: ~A~%" fd event-types) ;; TODO delme
             (with-lock-held-on-connection-multiplexer multiplexer
               (bind ((connection (aref (fd->connection-of multiplexer) fd)))
                 (when connection
                   (assert (not (eq event-types :error)) () "Oops, TODO: what does it mean, what should we do?")
                   (when (member (wait-reason-of connection) event-types :test #'eq)
                     (setf connection-ready-to-continue connection)
                     (unregister-connection multiplexer connection)))))
             (awhen connection-ready-to-continue
               (continue-connection multiplexer it))))))

#+nil
(defun busy-loop-hack (acceptor)
  ;; TODO this is a busy loop, only temporary until the epoll based
  ;; code is ready
  (loop
     (loop for connection :across (fd->connection-of acceptor) do
          (when connection
            (bind ((wait-reason (wait-reason-of connection)))
              (assert (or (eq wait-reason :read)
                          (eq wait-reason :wite)))
              (when (fd-ready-p (fd-of connection) (wait-reason-of connection))
                (continue-connection acceptor connection)))))
     (sb-unix:nanosleep 0 (* 500 1000000))
     (format t "tick~%")))
