;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; services.lisp --- Service lookup.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :net.sockets)

(defclass service ()
  ((name :initarg :name :reader service-name
         :documentation "The service name.")
   (port :initarg :port :reader service-port
          :documentation "The service's default port.")
   ;; why only these keyword? --luis
   (protocol :initarg :protocol :reader service-protocol
             :documentation "The service's protocol, :TCP or :UDP."))
  (:documentation "Class representing a service."))

(defun make-service (name port protocol)
  "Constructor for SERVICE objects."
  (make-instance 'service :name name :port port :protocol protocol))

(defmethod print-object ((service service) stream)
  (print-unreadable-object (service stream :type t :identity nil)
    (with-slots (name port protocol) service
      (format stream "Name: ~A Port: ~A Protocol: ~A" name port protocol))))

(defun lookup-service-number (service protocol))

(defun lookup-service-name (service protocol))

(define-condition unknown-service ()
  ((name :initarg :name :initform nil :reader service-name))
  (:report (lambda (condition stream)
             (format stream "Unknown service: ~S" (service-name condition))))
  (:documentation "Condition raised when a network service is not found."))

(defun lookup-service (service &optional (protocol :tcp))
  "Lookup a service by port or name.  PROTOCOL should be one
of :TCP, :UDP or :ANY."
  (check-type protocol (member :tcp :udp :any))
  (let* ((parsed-number (parse-number-or-nil service :ub16))
         (serv (if parsed-number
                   (lookup-service-number parsed-number protocol)
                   (lookup-service-name service protocol))))

    (or serv (error 'unknown-service :name service))))
