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

(defun make-connection-acceptor (name handler &key (fd-multiplexer-type 'epoll-multiplexer)
                                 worker-count (external-format :default))
  (make-instance 'connection-acceptor
                 :external-format external-format
                 :worker-count worker-count
                 :connection-handler handler
                 :name name
                 :fd-multiplexer-type fd-multiplexer-type))

(defmethod startup-acceptor ((self connection-acceptor) &key address port backlog)
  (unless (and address port)
    (error "Must specify which address and port to listen on using the :address and :port keyword arguments!"))
  (with-lock-held-on-connection-multiplexer self
    (when (accepting-connection-of self)
      (error "Acceptor ~A is already started" self))
    (bind ((socket (make-socket :connect :passive
                                :external-format (external-format-of self)))
           (done nil))
      ;; TODO use alexa:unprotcase
      (unwind-protect
           (progn
             (setf (fd-non-blocking socket) t)
             (bind-address socket (ensure-hostname address)
                           :port port
                           :reuse-address t)
             (listen-on socket :backlog backlog)
             (setf (accepting-connection-of self)
                   (make-instance 'accepting-connection
                                  :continuation (with-call/cc
                                                  (let/cc k
                                                    k)
                                                  (acceptor-accept-loop self))
                                  :wait-reason :read
                                  :socket socket))
             (startup-connection-multiplexer self)
             (register-connection self (accepting-connection-of self))
             (setf done t))
        (unless done
          (close socket :abort t)))))
  self)

(defun/cc acceptor-accept-loop (acceptor)
  (loop
     ;; TODO (shutdown-requested-p acceptor) call may not be neccessary here
     until (shutdown-requested-p acceptor) do
     (when (eq (accept-one-connection acceptor) :starving)
       (let/cc k
         (values k :read)))))

(defun accept-one-connection (acceptor)
  (with-sockaddr-storage (ss)
    (with-socklen (size size-of-sockaddr-storage)
      (bind ((socket (socket-of (accepting-connection-of acceptor)))
             (connection nil))
        (handler-case
            (setf connection
                  (make-connection (%accept (fd-of socket) ss size)
                                   :external-format (external-format-of socket)))
          (nix:ewouldblock ()
            (return-from accept-one-connection :starving)))
        (when connection
          (bind ((done nil))
            (unwind-protect
                 (block handling
                   (handler-bind ((serious-condition
                                   (lambda (error)
                                     (format *error-output*
                                             "Error while handling an incoming connection: ~A"
                                             error)
                                     (return-from handling))))
                     (handle-incoming-connection acceptor connection)
                     (setf done t)))
              (unless done
                (close connection))))))))
  (values))

(defun handle-incoming-connection (acceptor connection)
  (bind (((:values result wait-reason)
          (with-call/cc
            (setf (wait-reason-of connection) nil)
            (funcall (connection-handler-of acceptor)
                     connection)
            'finished)))
    (if (eq result 'finished)
        (progn
          (assert (not (connection-registered-p acceptor connection)))
          (close connection))
        (progn
          (setf (continuation-of connection) result)
          (setf (wait-reason-of connection) wait-reason)
          (with-lock-held-on-connection-multiplexer acceptor
            (register-connection acceptor connection))))))

(defmethod shutdown-acceptor ((self connection-acceptor) &key force)
  (with-lock-held-on-connection-multiplexer self
    (unwind-protect
         (progn
           (unregister-connection self (accepting-connection-of self))
           (shutdown-connection-multiplexer self))
      (close (accepting-connection-of self) :abort force)
      (setf (accepting-connection-of self) nil))))

(defmethod close ((self accepting-connection) &key abort)
  (close (socket-of self) :abort abort))

(defmethod fd-of ((self accepting-connection))
  (fd-of (socket-of self)))
