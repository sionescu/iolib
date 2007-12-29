;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; protocols.lisp --- Protocol lookup.
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

(defvar *protocols-file* "/etc/protocols")

(defclass protocol ()
  ((name :initarg :name :reader protocol-name
         :documentation "The protocol's primary name.")
   (aliases :initarg :aliases :reader protocol-aliases
            :documentation "A list of aliases for this protocol.")
   (number :initarg :number :reader protocol-number
           :documentation "The protocol number."))
  (:documentation "Class representing a protocol."))

(defun make-protocol (name number &optional aliases)
  "Constructor for PROTOCOL objects."
  (let ((number (cond ((numberp number) number)
                      ((string  number) (parse-integer number)))))
    (make-instance 'protocol :name name :number number :aliases aliases)))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity nil)
    (with-slots (name aliases number) protocol
      (format stream "Name: ~S Number: ~A Aliases: ~:[None~;~:*~{~S~^, ~}~]"
              name number aliases))))

(defun find-protocol-in-parsed-lines (tokens predicate)
  (when (< (length tokens) 2) (error 'parse-error))
  (destructuring-bind (name value &rest aliases) tokens
    (let ((value (parse-integer value)))
      (when (funcall predicate name value aliases)
        (make-protocol name value aliases)))))

(defun lookup-protocol-on-disk-by-name (file protocol)
  (flet ((good-proto-p (name value aliases)
           (declare (ignore value))
           (or (string= protocol name)
               (member protocol aliases :test #'string=))))
    (iterate ((tokens (serialize-etc-file file)))
      (ignore-some-conditions (parse-error)
        (let ((proto (find-protocol-in-parsed-lines tokens #'good-proto-p)))
          (when proto (return-from lookup-protocol-on-disk-by-name proto)))))))

(defun lookup-protocol-on-disk-by-number (file protocol)
  (flet ((good-proto-p (name value aliases)
           (declare (ignore name aliases))
           (= protocol value)))
    (iterate ((tokens (serialize-etc-file file)))
      (ignore-some-conditions (parse-error)
        (let ((proto (find-protocol-in-parsed-lines tokens #'good-proto-p)))
          (when proto (return-from lookup-protocol-on-disk-by-number proto)))))))

(define-condition unknown-protocol ()
  ((name :initarg :name :initform nil :reader unknown-protocol-name))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~S" (unknown-protocol-name condition))))
  (:documentation "Condition raised when a network protocol is not found."))

(defvar *protocols-cache-by-name*   (make-hash-table :test #'equal))
(defvar *protocols-cache-by-number* (make-hash-table :test #'eql))
(defvar *protocols-cache-lock* (bt:make-lock "/etc/protocols cache lock"))

(defun find-protocol (thing cache-fn disk-fn)
  (or (funcall cache-fn thing)
      (let ((protocol (funcall disk-fn *protocols-file* thing)))
        (when protocol
          (setf (gethash (protocol-name protocol) *protocols-cache-by-name*) protocol)
          (dolist (alias (protocol-aliases protocol))
            (setf (gethash alias *protocols-cache-by-name*) protocol))
          (setf (gethash (protocol-number protocol) *protocols-cache-by-number*) protocol)
          (values protocol)))))

(defun lookup-protocol-by-name (proto)
  (bt:with-lock-held (*protocols-cache-lock*)
    (find-protocol proto
                   #'(lambda (p) (gethash p *protocols-cache-by-name*))
                   #'lookup-protocol-on-disk-by-name)))

(defun lookup-protocol-by-number (proto)
  (bt:with-lock-held (*protocols-cache-lock*)
    (find-protocol proto
                   #'(lambda (p) (gethash p *protocols-cache-by-number*))
                   #'lookup-protocol-on-disk-by-number)))

(defun purge-protocols-cache (&optional file)
  (declare (ignore file))
  (map 'nil #'clrhash (list *protocols-cache-by-name*
                            *protocols-cache-by-number*)))

(defvar *protocols-monitor*
  (make-instance 'file-monitor
                 :file *protocols-file*
                 :update-fn 'purge-protocols-cache
                 :lock *protocols-cache-lock*))

(defun lookup-protocol (protocol)
  "Lookup a protocol by name or number.  Signals an
UNKNOWN-PROTOCOL error if no protocol is found."
  (check-type protocol (or unsigned-byte string keyword) "an unsigned-byte, a string or a keyword")
  (update-monitor *protocols-monitor*)
  (let ((protocol (or (and (keywordp protocol) (string-downcase protocol))
                      (parse-number-or-nil protocol)
                      protocol))
        (proto (etypecase protocol
                 (unsigned-byte (lookup-protocol-by-number protocol))
                 (string        (lookup-protocol-by-name protocol)))))
    (if proto (values (protocol-number proto)
                      (protocol-name proto)
                      (protocol-aliases proto))
        (error 'unknown-protocol :name protocol))))
