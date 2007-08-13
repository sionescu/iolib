;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-buffer.lisp - Manage a connection buffer.
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

(defclass io-buffer ()
  ((data :accessor data-of)
   (size :accessor size-of)
   (start :initarg :start :accessor start-of)
   (end :initarg :end :accessor end-of))
  (:default-initargs :start 0 :end 0))

(defvar *default-buffer-size* 4096)

(defmethod initialize-instance :after ((buffer io-buffer) &key
                                       (size *default-buffer-size*))
  (setf (data-of buffer) (make-array (or size *default-buffer-size*)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
        (size-of buffer) size))
