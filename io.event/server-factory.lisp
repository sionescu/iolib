;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; server-factory.lisp - TCP server factories.
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

;;; Server factory

(defclass server-factory ()
  ((protocol :initarg :protocol
             :accessor protocol-of)))

(defgeneric on-connection-received (factory event-base socket))

(defmethod on-connection-received ((factory server-factory)
                                   (event-loop event-base)
                                   (socket active-socket))
  )

;;; Event LOOP

(defclass event-loop (event-base)
  ((sockets :initform (make-hash-table :test #'eql)
            :accessor sockets-of)
   (protocols :initform (make-hash-table :test #'eql)
              :accessor protocols-of)))

(defgeneric listen-tcp (event-loop &key host port factory))

(defmethod listen-tcp ((event-loop event-loop)
                       &key host port factory)
  (check-type host address)
  (check-type port (unsigned-byte 16))
  (check-type factory server-factory)
  (let ((socket (make-socket :family (address-type host)
                             :type :stream :connect :passive
                             :local-host host :local-port port)))
    (setf (fd-non-blocking socket) t)
    (setf (gethash (fd-of socket) (sockets-of event-loop)) socket)
    (add-fd event-loop (fd-of socket) :read
            #'(lambda (fd event)
                (ecase event
                  (:read
                   (let ((peer (accept-connection socket)))
                     (when peer
                       (let* ((transport (make-instance 'tcp-transport
                                                        :event-loop event-loop
                                                        :socket socket))
                              (protocol (make-instance (protocol-of factory)
                                                       :transport transport)))
                         (setf (gethash (fd-of peer) (protocols-of event-loop))
                               (cons peer protocol))))))
                  (:error
                   (error "Got an error on the server socket: ~A~%" socket)))))))
