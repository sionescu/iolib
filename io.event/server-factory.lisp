;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; server-factory.lisp --- TCP server factories.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

;;;; Server

(defclass event-manager ()
  ()
  (:documentation ""))

(defclass protocol-manager-mixin ()
  ((protocol :initarg :protocol :accessor protocol-of))
  (:documentation ""))

#- (and)
(defmethod initialize-instance :after ((pmm protocol-manager-mixin) &key)
  (with-slots (protocol) pmm
    (unless (typep protocol 'io-protocol)
      (setq protocol (make-instance protocol)))))

(defclass server (event-manager protocol-manager-mixin)
  ()
  (:documentation ""))

(defclass tcp-server (server)
  ((connections :initform nil :accessor connections-of))
  (:documentation ""))

(defmethod print-object ((tcp-server server) stream)
  (print-unreadable-object (tcp-server stream :type t :identity t)
    (format stream "~A connection" (length (connections-of tcp-server)))))

;;; wtf?
#- (and)
(defgeneric on-connection-received (factory event-base socket))

#- (and)
(defmethod on-connection-received ((factory server-factory)
                                   (event-loop event-base)
                                   (socket active-socket))
  )

(defclass client (event-manager protocol-manager-mixin)
  ()
  (:documentation ""))

;;;; Event Loop

(defclass event-loop (event-base)
  ((sockets :initform (make-hash-table :test #'eql)
            :accessor sockets-of)
   (protocols :initform (make-hash-table :test #'eql)
              :accessor protocols-of)))

(defun listen-tcp (event-loop &key host port server)
  (check-type event-loop event-loop)
  (check-type port (unsigned-byte 16))
  (check-type server tcp-server)
  (let* ((host (ensure-address host))
         (socket (make-socket :family (address-type host)
                              :type :stream :connect :passive
                              :local-host host :local-port port)))
    (setf (fd-non-blocking socket) t)
    (setf (gethash (fd-of socket) (sockets-of event-loop)) socket)
    (add-fd event-loop (fd-of socket) :read
            (lambda (fd event)
              (declare (ignore fd))
              (ecase event
                (:read
                 (let ((peer (accept-connection socket)))
                   (when peer
                     ;; The transport sets things up.
                     (let* ((transport (change-class peer 'tcp-transport
                                                     :event-loop event-loop))
                            (protocol (make-instance (protocol-of server)
                                                     :transport transport)))
                       ;; how tricky (or how bad an idea) would it be
                       ;; to have the transport, socket and
                       ;; protocol/connection all be the same object?
                       (setf (protocol-of transport) protocol)
                       ;; why save protocols in the event-loop?
                       ;; (setf (gethash (fd-of peer) (protocols-of event-loop))
                       ;;       (cons peer protocol))
                       (push protocol (connections-of server))
                       #- (and)
                       (add-fd event-loop (fd-of peer) :read
                               (lambda (fd event)
                                 (declare (ignore fd))
                                 (ecase event
                                   (:read
                                    (on-data-received protocol
                                                      #(104 101 108 108 111 33 13 10)))
                                   (:error
                                    (warn "connection error")))))
                       ))))
                (:error
                 (error "Got an error on the server socket: ~A~%" socket)))))))

(defvar *default-event-loop* (make-instance 'event-loop))

;;; quick hack
(defun run-tcp-server (server &rest listen-tcp-args)
  (apply #'listen-tcp *default-event-loop*
         :server (make-instance server)
         listen-tcp-args)
  (event-dispatch *default-event-loop*))
