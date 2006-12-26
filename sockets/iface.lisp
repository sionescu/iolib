;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

(defclass interface ()
  ((name  :initarg :name
          :initform (error "The interface must have a name.")
          :reader interface-name)
   (index :initarg :index
          :initform (error "The interface must have an index.")
          :reader interface-index)))

(defmethod print-object ((iface interface) stream)
  (print-unreadable-object (iface stream :type nil :identity nil)
    (with-slots (name id) iface
      (format stream "Network Interface: ~S. Index: ~A"
              (interface-name iface) (interface-index iface)))))

(defun make-interface (name index)
  (make-instance 'interface
                 :name  name
                 :index index))

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

(defun get-network-interfaces ()
  (with-foreign-object (ifptr :pointer)
    (setf ifptr (et:if-nameindex))
    (unless (null-pointer-p ifptr)
      (let* ((iflist
              (loop
                 :for i :from 0
                 :for name := (foreign-slot-value (mem-aref ifptr 'et:if-nameindex i)
                                                  'et:if-nameindex 'et:name)
                 :for index := (foreign-slot-value (mem-aref ifptr 'et:if-nameindex i)
                                                   'et:if-nameindex 'et:index)
                 :while (plusp index)
                 :collect (make-interface name index)
                 :finally (et:if-freenameindex ifptr))))
        iflist))))

(defun get-interface-by-index (index)
  (check-type index unsigned-byte "an unsigned integer")
  (with-foreign-object (buff :uint8 et:ifnamesize)
    (let (retval)
      (handler-case
          (setf retval (et:if-indextoname index buff))
        (et:unix-error-nxio (err)
          (error 'unknown-interface
                 :code (error-code err)
                 :identifier (error-identifier err)
                 :index index)))
      (make-interface (copy-seq retval) index))))

(defun get-interface-by-name (name)
  (check-type name string "a string")
  (let (retval)
    (handler-case
        (setf retval (et:if-nametoindex name))
      (et:unix-error-nodev (err)
        (error 'unknown-interface
               :code (error-code err)
               :identifier (error-identifier err)
               :name name)))
    (make-interface (copy-seq name) retval)))

(defun lookup-interface (iface)
  (let ((parsed-number (parse-number-or-nil iface)))
    (if parsed-number
        (get-interface-by-index parsed-number)
        (get-interface-by-name iface))))
