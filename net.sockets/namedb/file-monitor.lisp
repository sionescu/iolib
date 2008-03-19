;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; file-monitor.lisp --- Monitor files on disk.
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

(in-package :net.sockets)

(defclass file-monitor ()
  ((file :initform (error "Must supply a file name")
         :initarg :file :accessor file-of)
   (timestamp :initarg :timestamp :accessor timestamp-of)
   (update-fn :initarg :update-fn :accessor update-fn-of)
   (lock      :initarg :lock      :accessor lock-of))
  (:default-initargs :timestamp 0))

(defmethod print-object ((monitor file-monitor) stream)
  (print-unreadable-object (monitor stream :type nil :identity nil)
    (format stream "File monitor for ~S" (file-of monitor))))

(defun monitor-oldp (monitor)
  (declare (type file-monitor monitor))
  (let ((mtime (file-write-date (file-of monitor))))
    (values (< (timestamp-of monitor) mtime)
            mtime)))

(defgeneric update-monitor (monitor)
  (:method ((monitor file-monitor))
    (bt:with-lock-held ((lock-of monitor))
      (multiple-value-bind (oldp mtime) (monitor-oldp monitor)
        (when oldp
          (funcall (update-fn-of monitor) (file-of monitor))
          (multiple-value-prog1
              (values (timestamp-of monitor) mtime)
            (setf (timestamp-of monitor) mtime)))))))
