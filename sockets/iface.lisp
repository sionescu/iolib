;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; iface.lisp --- Network interface class and operators.
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

(defclass interface ()
  ((name  :initarg :name :reader interface-name
          :initform (error "The interface must have a name.")
          :documentation "The interface's name.")
   (index :initarg :index :reader interface-index
          :initform (error "The interface must have an index.")
          :documentation "The interface's index number."))
  (:documentation "Class describing a network interface."))

(defmethod print-object ((iface interface) stream)
  (print-unreadable-object (iface stream :type nil :identity nil)
    (with-slots (name id) iface
      (format stream "Network Interface: ~S Index: ~A"
              (interface-name iface) (interface-index iface)))))

(defun make-interface (name index)
  "Constructor for INTERFACE objects."
  (make-instance 'interface :name  name :index index))

(define-condition unknown-interface (system-error)
  ((name  :initarg :name  :initform nil :reader interface-name)
   (index :initarg :index :initform nil :reader interface-index))
  (:report (lambda (condition stream)
             (if (interface-name condition)
                 (format stream "Unknown interface: ~A"
                         (interface-name condition))
                 (format stream "Unknown interface index: ~A"
                         (interface-index condition)))))
  (:documentation "Condition raised when a network interface is not found."))

(defun list-network-interfaces ()
  "Returns a list of network interfaces currently available."
  (with-foreign-object (ifptr :pointer)
    (setf ifptr (if-nameindex))
    (unless (null-pointer-p ifptr)
      (loop :for i :from 0
            :for name := (foreign-slot-value
                          (mem-aref ifptr 'if-nameindex i)
                                 'if-nameindex 'name)
            :for index := (foreign-slot-value
                           (mem-aref ifptr 'if-nameindex i)
                                  'if-nameindex 'index)
            :while (plusp index)
            :collect (make-interface name index)
            :finally (if-freenameindex ifptr)))))

(defun get-interface-by-index (index)
  (check-type index unsigned-byte "an unsigned integer")
  (with-foreign-object (buff :uint8 ifnamesize)
    (let (retval)
      (handler-case
          (setf retval (if-indextoname index buff))
        (nix:enxio (err)
          (error 'unknown-interface
                 :code (osicat-sys:system-error-code err)
                 :identifier (osicat-sys:system-error-identifier err)
                 :index index)))
      (make-interface (copy-seq retval) index))))

(defun get-interface-by-name (name)
  (check-type name string "a string")
  (let (retval)
    (handler-case
        (setf retval (if-nametoindex name))
      (nix:enodev (err)
        (error 'unknown-interface
               :code (osicat-sys:system-error-code err)
               :identifier (osicat-sys:system-error-identifier err)
               :name name)))
    (make-interface (copy-seq name) retval)))

;;; Why bother parsing "22" as a number? --luis
(defun lookup-interface (iface)
  "Lookup an interface by name or index.  UNKNOWN-INTERFACE is
signalled if an interface is not found."
  (let ((parsed-number (parse-number-or-nil iface)))
    (if parsed-number
        (get-interface-by-index parsed-number)
        (get-interface-by-name iface))))
