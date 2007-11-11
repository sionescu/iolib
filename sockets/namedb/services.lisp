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

(defvar *services-file* "/etc/services")

(defclass service ()
  ((name :initarg :name :reader service-name
         :documentation "The service name.")
   (port :initarg :port :reader service-port
         :documentation "The service's default port.")
   (protocol :initarg :protocol :reader service-protocol
             :documentation "The service's protocol, :TCP or :UDP."))
  (:documentation "Class representing a service."))

(defun make-service (name port protocol)
  "Constructor for SERVICE objects."
  (let ((port (cond ((numberp port) port)
                    ((string  port) (parse-integer port))))
        (protocol (cond ((keywordp protocol) protocol)
                        ((stringp protocol)  (make-keyword
                                              (string-upcase protocol))))))
    (make-instance 'service :name name :port port :protocol protocol)))

(defmethod print-object ((service service) stream)
  (print-unreadable-object (service stream :type t :identity nil)
    (with-slots (name port protocol) service
      (format stream "Name: ~A Port: ~A Protocol: ~A" name port protocol))))

(defun split-port/proto (port/proto)
  (let ((pos (position #\/ port/proto)))
    (unless pos (error 'parse-error))
    (values (subseq port/proto 0 pos)
            (subseq port/proto (1+ pos)))))

(defun protocol-compatible-p (protocol thing)
  (case protocol
    (:any t)
    (:tcp (eq :tcp (make-keyword (string-upcase thing))))
    (:udp (eq :udp (make-keyword (string-upcase thing))))))

(defun find-service (tokens predicate)
  (when (< (length tokens) 2) (error 'parse-error))
  (destructuring-bind (name port/proto &rest aliases) tokens
    (multiple-value-bind (port proto) (split-port/proto port/proto)
      (when (funcall predicate name port proto aliases)
        (make-service name port proto)))))

(defun lookup-service-on-disk-by-number (file service protocol)
  (flet ((good-proto-p (name port proto aliases)
           (declare (ignore name aliases))
           (let ((pnum (parse-integer port)))
             (and (protocol-compatible-p protocol proto)
                  (= pnum service)))))
    (iterate ((tokens (serialize-etc-file file)))
      (ignore-some-conditions (parse-error)
        (let ((proto (find-service tokens #'good-proto-p)))
          (when proto (return-from lookup-service-on-disk-by-number
                        proto)))))))

(defun lookup-service-on-disk-by-name (file service protocol)
  (flet ((good-proto-p (name port proto aliases)
           (declare (ignore port))
           (and (protocol-compatible-p protocol proto)
                (or (string= service name)
                    (member service aliases :test #'string=)))))
    (iterate ((tokens (serialize-etc-file file)))
      (ignore-some-conditions (parse-error)
        (let ((proto (find-service tokens #'good-proto-p)))
          (when proto (return-from lookup-service-on-disk-by-name
                        proto)))))))

(define-condition unknown-service ()
  ((name :initarg :name :initform nil :reader service-name))
  (:report (lambda (condition stream)
             (format stream "Unknown service: ~S" (service-name condition))))
  (:documentation "Condition raised when a network service is not found."))

(defvar *tcp-services-cache-by-name*   (make-hash-table :test #'equal))
(defvar *tcp-services-cache-by-number* (make-hash-table :test #'eql))
(defvar *udp-services-cache-by-name*   (make-hash-table :test #'equal))
(defvar *udp-services-cache-by-number* (make-hash-table :test #'eql))

(defun find-service-name-in-cache (thing protocol)
  (ecase protocol
    (:tcp (gethash thing *tcp-services-cache-by-name*))
    (:udp (gethash thing *udp-services-cache-by-name*))
    (:any (or (gethash thing *tcp-services-cache-by-name*)
              (gethash thing *udp-services-cache-by-name*)))))

(defun find-service-number-in-cache (thing protocol)
  (ecase protocol
    (:tcp (gethash thing *tcp-services-cache-by-number*))
    (:udp (gethash thing *udp-services-cache-by-number*))
    (:any (or (gethash thing *tcp-services-cache-by-number*)
              (gethash thing *udp-services-cache-by-number*)))))

(declaim (inline get-service-cache))
(defun get-service-cache (protocol type)
  (ecase type
    (:name (ecase protocol
             (:tcp *tcp-services-cache-by-name*)
             (:udp *udp-services-cache-by-name*)))
    (:number (ecase protocol
               (:tcp *tcp-services-cache-by-number*)
               (:udp *udp-services-cache-by-number*)))))

(defun lookup-service-by-name (thing protocol)
  (or (find-service-name-in-cache thing protocol)
      (let ((service (lookup-service-on-disk-by-name
                      *services-file* thing protocol)))
        (when service
          (setf (gethash thing (get-service-cache (service-protocol service) :name))
                service)
          (values service)))))

(defun lookup-service-by-number (thing protocol)
  (or (find-service-number-in-cache thing protocol)
      (let ((service (lookup-service-on-disk-by-number
                      *services-file* thing protocol)))
        (when service
          (setf (gethash thing (get-service-cache (service-protocol service) :number))
                service)
          (values service)))))

(defun lookup-service (service &optional (protocol :tcp))
  "Lookup a service by port or name.  PROTOCOL should be one
of :TCP, :UDP or :ANY."
  (check-type protocol (member :tcp :udp :any))
  (let* ((parsed-number (parse-number-or-nil service :ub16))
         (serv (if parsed-number
                   (lookup-service-by-number parsed-number protocol)
                   (lookup-service-by-name service protocol))))
    (or serv (error 'unknown-service :name service))))

(defun purge-services-cache ()
  (map 'nil #'clrhash (list *tcp-services-cache-by-name*
                            *tcp-services-cache-by-number*
                            *udp-services-cache-by-name*
                            *udp-services-cache-by-number*)))

(defvar *services-monitor*
  (make-instance 'file-monitor
                 :file *services-file*
                 :update-fn 'purge-services-cache))
