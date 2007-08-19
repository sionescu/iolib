;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; server-factory.lisp --- TCP server factories.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package :io.event)

;;;; Base Factory Classes

(defclass factory ()
  ((protocol :initarg :protocol :accessor protocol-of)
   (event-base :initarg :event-base :accessor event-base-of))
  (:documentation "Factories manage stuff."))

;;;; Server

(defclass server (factory)
  ()
  (:documentation ""))

(defclass network-server (server)
  ((default-local-port :initform 0 :initarg :default-local-port
                       :accessor default-local-port-of))
  (:documentation ""))

;;;; TCP Server

(defclass tcp-server (network-server)
  ((connections :initform nil :accessor connections-of))
  (:documentation ""))

(defmethod print-object ((server tcp-server) stream)
  (print-unreadable-object (server stream :type t :identity t)
    (format stream "~A connections" (length (connections-of server)))))

(defgeneric on-server-connection-received (tcp-server event-base peer)
  (:documentation ""))

(defmethod on-server-connection-received ((server tcp-server) (eb event-base)
                                          peer)
  "Default main method for TCP-SERVERs.  Instantiates a new
PROTOCOL and respective TRANSPORT, sets them up and pushes the
new PROTOCOL onto the SERVER's connection list."
  (let* ((transport (make-instance 'tcp-transport :event-base eb :socket peer))
         (protocol (make-instance (protocol-of server) :transport transport)))
    (setf (protocol-of transport) protocol)
    (push protocol (connections-of server))))

;;; Badly named, maybe.
(defgeneric on-server-connection-error (tcp-server event-base)
  (:documentation "")
  (:method ((server tcp-server) event-base)
    (declare (ignore event-base))
    (warn "Got an error on the server socket: ~S" server)))

(defgeneric listen-tcp (server event-base &rest socket-options)
  (:documentation ""))

(defmethod listen-tcp ((server tcp-server) (base event-base)
                       &rest socket-options)
  (let ((socket (apply #'make-socket
                       :connect :passive
                       :local-port (getf socket-options :local-port
                                         (default-local-port-of server))
                       :local-host (getf socket-options :local-host
                                         +ipv4-unspecified+)
                       socket-options)))
    (setf (fd-non-blocking socket) t)
    (add-fd base (fd-of socket) :read
            (lambda (fd event)
              (declare (ignore fd))
              (ecase event
                (:read
                 (let ((peer (accept-connection socket)))
                   (when peer
                     (on-server-connection-received server base peer))))
                (:error
                 (on-server-connection-error server base)))))
    socket))

;; (defvar *default-event-base* (make-instance 'event-base))

;;; testing
(defun run-tcp-server (server &rest socket-options)
  (let ((event-base (make-instance 'event-base)))
    (apply 'listen-tcp (make-instance server) event-base socket-options)
    (event-dispatch event-base)))

;;;; UDP Server

(defclass udp-server (network-server)
  ((datagram-protocol :accessor datagram-protocol-of))
  (:documentation ""))

(defgeneric listen-udp (server event-base &rest socket-options)
  (:documentation ""))

(defmethod listen-udp ((server udp-server) (base event-base)
                       &rest socket-options)
  (let ((socket (apply #'make-socket
                       :type :datagram
                       :local-port (getf socket-options :local-port
                                         (default-local-port-of server))
                       :local-host (getf socket-options :local-host
                                         +ipv4-unspecified+)
                       socket-options)))
    (setf (fd-non-blocking socket) t)
    (let* ((transport (make-instance
                       'udp-transport :event-base base :socket socket))
           (protocol (make-instance (protocol-of server) :transport transport)))
      (setf (protocol-of transport) protocol
            (datagram-protocol-of server) protocol))
    socket))

;;; testing
(defun run-udp-server (server &rest socket-options)
  (let ((event-base (make-instance 'event-base)))
    (apply 'listen-udp (make-instance server) event-base socket-options)
    (event-dispatch event-base)))

;;;; Client

(defvar *current-event-base*)

;;; KLUDGE: think this through.
(defun init-default-event-base (&rest options)
  (setq *current-event-base*
        (apply #'make-instance 'event-base options)))

(defclass client (factory)
  ()
  (:documentation ""))

;;; KLUDGE: think this through.
(defmethod event-base-of ((client client))
  *current-event-base*)

(defclass network-client (client)
  ((default-remote-port :initform 0 :initarg :default-remote-port
                        :accessor default-remote-port-of))
  (:documentation ""))

;;; Could eventually share stuff with TCP-SERVER.
(defclass tcp-client (network-client)
  ((connections :initform nil :accessor connections-of))
  (:documentation ""))

(defgeneric add-connection (client protocol &rest socket-options))

(defmethod add-connection ((client tcp-client) protocol &rest options)
  (let ((trans (make-instance 'tcp-transport
                              :event-base (event-base-of client)
                              :socket (apply #'make-socket options)
                              :protocol protocol)))
    (setf (transport-of protocol) trans)))

;;;; Deferred

(defclass deferred-mixin ()
  ((deferred :initarg :deferred :accessor deferred-of))
  (:documentation ""))

(defclass deferred ()
  ((result-callback :accessor result-callback-of
                    :initform
                    (lambda (&rest values)
                      (warn "Unhandled deferred callback: ~S" values))
                    :documentation "")
   (error-callback :accessor error-callback-of :initform #'error
                   :documentation ""))
  (:documentation ""))

;;; Any better syntax suggestions?
(defmacro with-async-handler (return-vars form error-clauses &body body)
  (with-unique-names (result-deferred)
    `(let ((,result-deferred ,form))
       (setf (result-callback-of ,result-deferred)
             (lambda ,return-vars ,@body))
       (setf (error-callback-of ,result-deferred)
             (lambda (some-error)
               (handler-case
                   (error some-error)
                 ,@error-clauses)))
       ,result-deferred)))

;;; This macro is potentially very confusing for the user.  Depending
;;; on whether *CURRENT-EVENT-BASE* is bound it'll either return the
;;; deferred object or run an event loop and actually return the value
;;; (or signal an error) instead.  On that note, maybe
;;; WITH-ASYNC-HANDLER should check whether it actually got a deferred
;;; object.
(defmacro with-deferred-result (() &body body)
  (with-unique-names (body-fn)
    `(flet ((,body-fn () ,@body))
       (if (boundp '*current-event-base*)
           (,body-fn)
           (call-synchronously-with-fresh-event-base #',body-fn)))))

(defun call-synchronously-with-fresh-event-base (function)
  (with-event-base (*current-event-base* :exit-when-empty t)
    (let (return-values error)
      (with-async-handler (&rest values) (funcall function)
          ((error (c) (setq error c)))
        (setq return-values values))
      (event-dispatch *current-event-base*)
      (if error
          (error error)
          (apply #'values return-values)))))
