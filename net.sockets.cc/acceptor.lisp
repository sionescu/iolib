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

(defconstant +event-polling-timeout-in-seconds+ 3)

(defmacro with-lock-held-on-acceptor (acceptor &body body)
  `(bordeaux-threads:with-lock-held ((lock-of ,acceptor))
     ,@body))

(defun make-connection-acceptor (handler &key worker-count (external-format :default))
  (make-instance 'connection-acceptor
                 :external-format external-format
                 :worker-count worker-count
                 :connection-handler handler))

(defun make-connection (fd &key external-format input-buffer-size
                        output-buffer-size)
  (bind ((result (make-instance 'connection
                                :file-descriptor fd
                                :external-format external-format
                                :input-buffer-size input-buffer-size
                                :output-buffer-size output-buffer-size)))
    (assert (fd-of result))
    result))

(defmethod startup-acceptor ((acceptor connection-acceptor) &key address port backlog
                             (multiplexer-type 'epoll-multiplexer))
  (unless (and address port)
    (error "Must specify which address and port to listen on using the :address and :port keyword arguments!"))
  (bind ((multiplexer nil))
    (with-lock-held-on-acceptor acceptor
      (when (accepting-connection-of acceptor)
        (error "Acceptor ~A is already started" acceptor))
      (setf multiplexer (make-instance 'connection-multiplexer
                                       :fd-multiplexer-type multiplexer-type))
      (setf (connection-multiplexer-of acceptor) multiplexer)
      (bind ((socket (make-socket :connect :passive
                                           :external-format (external-format-of acceptor)))
             (done nil))
        (unwind-protect
             (progn
               (setf (fd-non-blocking socket) t)
               (bind-address socket (ensure-hostname address)
                             :port port
                             :reuse-address t)
               (listen-on socket :backlog backlog)
               (setf (accepting-connection-of acceptor)
                     (make-instance 'accepting-connection
                                    :continuation (with-call/cc
                                                    (let/cc k
                                                      k)
                                                    (acceptor-accept-loop acceptor))
                                    :wait-reason :read
                                    :socket socket))
               (loop
                  repeat (worker-count-of acceptor) do
                  (spawn-acceptor-worker-thread acceptor))
               (setf done t))
          (unless done
            (close socket)))))
    (with-lock-held-on-connection-multiplexer multiplexer
      (register-connection multiplexer (accepting-connection-of acceptor))))
  acceptor)

(defun spawn-acceptor-worker-thread (acceptor)
  (bind ((worker-thread nil))
    ;; this here has a (theoretical) race condition, because MAKE-THREAD
    ;; also starts the thread right away.
    (setf worker-thread
          (bordeaux-threads:make-thread
           (lambda ()
             (bind ((done nil))
               (unwind-protect
                    (progn
                      (acceptor-worker-loop acceptor)
                      (setf done t))
                 (deletef (workers-of acceptor) worker-thread)
                 (unless done
                   ;; TODO proper error handling and debugging helpers
                   (warn "An acceptor worker died")))))
           :name (format nil "acceptor worker ~A" (length (workers-of acceptor)))))
    (vector-push-extend worker-thread (workers-of acceptor))))

(defun acceptor-worker-loop (acceptor)
  (bind ((multiplexer (connection-multiplexer-of acceptor)))
    (loop
       named looping do
       (when (shutdown-requested-p acceptor)
         (return-from looping))
       (bind ((events (harvest-events multiplexer +event-polling-timeout-in-seconds+)))
         (loop
            for entry :in events do
            (bind (((fd event-types) entry)
                   (connection-ready-to-continue nil))
              (format t "fd: ~A, event: ~A~%" fd event-types)
              (with-lock-held-on-connection-multiplexer multiplexer
                (bind ((connection (aref (fd->connection-of multiplexer) fd)))
                  (when connection
                    (assert (not (eq event-types :error)) () "Oops, TODO: what does it mean, what should we do?")
                    (when (member (wait-reason-of connection) event-types :test #'eq)
                      (setf connection-ready-to-continue connection)
                      (unregister-connection multiplexer connection)))))
              (awhen connection-ready-to-continue
                (continue-connection multiplexer it))))))))

(defun/cc acceptor-accept-loop (acceptor)
  (bind ((fd (fd-of (accepting-connection-of acceptor))))
    (loop
         ;; TODO (shutdown-requested-p acceptor) may not be neccessary here
       until (shutdown-requested-p acceptor) do
       (wait-until-fd-ready/cc fd :read)
       (accept-one-connection acceptor))))

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
            ;; some other thread may have accepted this connection before us, but that's ok.
            ))
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
                (close connection)))))))))

(defun handle-incoming-connection (acceptor connection)
  (bind ((multiplexer (connection-multiplexer-of acceptor))
         ((:values result wait-reason)
          (with-call/cc
            (setf (wait-reason-of connection) nil)
            (funcall (connection-handler-of acceptor)
                     connection)
            'finished)))
    (if (eq result 'finished)
        (progn
          (assert (not (connection-registered-p multiplexer connection)))
          (close connection))
        (progn
          (setf (continuation-of connection) result)
          (setf (wait-reason-of connection) wait-reason)
          (with-lock-held-on-connection-multiplexer multiplexer
            (register-connection multiplexer connection))))))

(defmethod shutdown-acceptor ((acceptor connection-acceptor) &key force)
  (setf (shutdown-requested-p acceptor) t)
  ;; TODO instead of this loop we could use a conditional variable, but is it worth the trouble?
  (flet ((close-accepting-connection-and-clear ()
           (close (accepting-connection-of acceptor) :abort force)
           (setf (accepting-connection-of acceptor) nil)))
    (unwind-protect
         (loop
            named looping do
            (with-lock-held-on-acceptor acceptor
              (when (zerop (length (workers-of acceptor)))
                (when (accepting-connection-of acceptor)
                  (bind ((multiplexer (connection-multiplexer-of acceptor)))
                    (with-lock-held-on-connection-multiplexer multiplexer
                      (unregister-connection multiplexer (accepting-connection-of acceptor)))
                    (close-connection-multiplexer multiplexer))
                  (setf (connection-multiplexer-of acceptor) nil)
                  (close-accepting-connection-and-clear))
                (return-from looping)))
            (sleep 1))
      (when (accepting-connection-of acceptor)
        (close-accepting-connection-and-clear)))))

(defmethod close ((self accepting-connection) &key abort)
  (close (socket-of self) :abort abort))

(defmethod fd-of ((self accepting-connection))
  (fd-of (socket-of self)))
