;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
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

(defmacro with-lock-held-on-connection-multiplexer (multiplexer &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock-of ,multiplexer))
     ,@body))

(defmethod initialize-instance :after ((self connection-multiplexer) &key fd-multiplexer-type)
  (assert fd-multiplexer-type)
  (setf (fd-multiplexer-of self)
        (make-instance fd-multiplexer-type)))

(defmethod register-connection ((multiplexer connection-multiplexer) (connection connection-with-continuation-mixin))
  (assert (continuation-of connection))
  (assert (wait-reason-of connection))
  (with-lock-held-on-connection-multiplexer multiplexer
    (bind ((fd (fd-of connection))
           (fd->connection (fd->connection-of multiplexer))
           (mux (fd-multiplexer-of multiplexer))
           (wait-reason (wait-reason-of connection)))
      (assert (and fd (not (zerop fd))))
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

(defmethod close-connection-multiplexer ((self connection-multiplexer))
  (loop
     with fd->connection = (fd->connection-of self)
     for idx :from 0 :below (length fd->connection) do
       (unless (null (aref fd->connection idx))
         (warn "A connection is still registered for fd ~A while shutting down ~A" idx self)))
  (io.multiplex::close-multiplexer (fd-multiplexer-of self)))

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
  ;; TODO locking?
  (harvest-events (fd-multiplexer-of connection-multiplexer) timeout))

(defun busy-loop-hack (acceptor)
  ;; TODO this is a busy loop, only temporary until the epoll based
  ;; code is ready
  (bind ((multiplexer (connection-multiplexer-of acceptor)))
    (loop
       (loop for connection :across (fd->connection-of multiplexer) do
            (when connection
              (bind ((wait-reason (wait-reason-of connection)))
                (assert (or (eq wait-reason :read)
                            (eq wait-reason :wite)))
                (when (fd-ready-p (fd-of connection) (wait-reason-of connection))
                  (continue-connection multiplexer connection)))))
       (sb-unix:nanosleep 0 (* 500 1000000))
       (format t "tick~%"))))
