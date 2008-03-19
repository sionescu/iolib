;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; iface.lisp --- Network interface class and operators.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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
  ((name  :initarg :name :reader interface-name)
   (index :initarg :index :reader interface-index))
  (:documentation "Class describing a network interface."))

(defmethod print-object ((interface interface) stream)
  (print-unreadable-object (interface stream :type nil :identity nil)
    (with-slots (name index) interface
      (format stream "Network Interface: ~S Index: ~A" name index))))

(defun make-interface (name index)
  "Constructor for INTERFACE objects."
  (make-instance 'interface :name name :index index))

(define-condition unknown-interface (system-error)
  ((datum :initarg :datum :initform nil :reader unknown-interface-datum))
  (:report (lambda (condition stream)
             (format stream "Unknown interface: ~A"
                     (unknown-interface-datum condition))))
  (:documentation "Condition raised when a network interface is not found."))

(defun signal-unknown-interface-error (system-error datum)
  (error 'unknown-interface
         :code (osicat-sys:system-error-code system-error)
         :identifier (osicat-sys:system-error-identifier system-error)
         :datum datum))

(defun list-network-interfaces ()
  "Returns a list of network interfaces currently available."
  (let ((ifptr (null-pointer)))
    (macrolet ((%if-slot-value (slot index)
                 `(foreign-slot-value
                   (mem-aref ifptr 'if-nameindex ,index)
                   'if-nameindex ,slot)))
      (unwind-protect
           (progn
             (setf ifptr (%if-nameindex))
             (loop :for i :from 0
                   :for name := (%if-slot-value 'name i)
                   :for index := (%if-slot-value 'index i)
               :while (plusp index) :collect (make-interface name index)))
        (unless (null-pointer-p ifptr) (%if-freenameindex ifptr))))))

(defun get-interface-by-index (index)
  (with-foreign-object (buffer :uint8 ifnamesize)
    (handler-case
        (%if-indextoname index buffer)
      (nix:enxio (error)
        (signal-unknown-interface-error error index))
      (:no-error (name)
        (make-interface name index)))))

(defun get-interface-by-name (name)
  (handler-case
      (%if-nametoindex name)
    (nix:enxio (error)
      (signal-unknown-interface-error error name))
    (:no-error (index)
      (make-interface (copy-seq name) index))))

(defun lookup-interface (interface)
  "Lookup an interface by name or index.  UNKNOWN-INTERFACE is
signalled if an interface is not found."
  (check-type interface (or unsigned-byte string symbol) "non-negative integer, a string or a symbol")
  (let ((interface (ensure-string-or-unsigned-byte interface)))
    (etypecase interface
      (unsigned-byte (get-interface-by-index interface))
      (string        (get-interface-by-name interface)))))
