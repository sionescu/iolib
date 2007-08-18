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
  ()
  (:documentation "Factories manage stuff."))

(defclass protocol-factory-mixin ()
  ((protocol :initarg :protocol :accessor protocol-of))
  (:documentation ""))

;;;; Server

(defclass server (factory protocol-factory-mixin)
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

(defmethod print-object ((tcp-server server) stream)
  (print-unreadable-object (tcp-server stream :type t :identity t)
    (format stream "~A connection" (length (connections-of tcp-server)))))

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

(defclass client (factory protocol-factory-mixin)
  ()
  (:documentation ""))
