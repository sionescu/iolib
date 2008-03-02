;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; classes.lisp --- fd-streams classes.
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

(in-package :io.streams)

;;;; Types

(deftype ub8  () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype sb8  () '(signed-byte 8))
(deftype sb16 () '(signed-byte 16))
(deftype sb32 () '(signed-byte 32))

(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))

(deftype ub8-vector () '(vector ub8))

(deftype ub16-sarray (&optional (size '*))
  `(simple-array ub16 (,size)))

;;;; Socket Buffers

(deftype stream-buffer () 'foreign-pointer)
(deftype buffer-index () '(unsigned-byte 24))

(defstruct (iobuf (:constructor %make-iobuf ()))
  (data (null-pointer) :type stream-buffer)
  (size 0 :type buffer-index)
  (start 0 :type buffer-index)
  (end 0 :type buffer-index))

;;;; File-Descriptor Mixins

(deftype stream-position () '(unsigned-byte 64))

(defun default-read-fn (fd buf nbytes)
  (nix:read fd buf nbytes))

(defun default-write-fn (fd buf nbytes)
  (nix:write fd buf nbytes))

(defclass dual-channel-fd-mixin ()
  ((input-fd  :initform nil :initarg :input-fd :accessor input-fd-of
              :documentation "placeholder")
   (read-fn :initform 'default-read-fn :initarg :read-fn :accessor read-fn-of)
   (output-fd :initform nil :initarg :output-fd :accessor output-fd-of
              :documentation "placeholder")
   (write-fn :initform 'default-write-fn :initarg :write-fn :accessor write-fn-of))
  (:documentation "placeholder"))

(defgeneric input-fd-non-blocking (socket)
  (:documentation "placeholder"))

(defgeneric (setf input-fd-non-blocking) (mode fd-mixin))

(defgeneric output-fd-non-blocking (socket)
  (:documentation "placeholder"))

(defgeneric (setf output-fd-non-blocking) (mode fd-mixin))

(defclass dual-channel-single-fd-mixin (dual-channel-fd-mixin)
  ()
  (:documentation "placeholder"))

(defgeneric fd-of (stream)
  (:documentation "placeholder")
  (:method ((stream dual-channel-single-fd-mixin))
    (with-accessors ((fd-in  input-fd-of)
                     (fd-out output-fd-of)) stream
      (assert (eql fd-in fd-out) (fd-in fd-out)
              "Input and output FDs must be equal: ~A, ~A" fd-in fd-out)
      (values fd-in))))

(defgeneric (setf fd-of) (fd stream)
  (:documentation "placeholder")
  (:method (fd (stream dual-channel-single-fd-mixin))
    (with-accessors ((fd-in  input-fd-of)
                     (fd-out output-fd-of)) stream
      (setf fd-in fd fd-out fd)
      (values fd-in))))

(defgeneric fd-non-blocking (fd-mixin)
  (:documentation "placeholder"))

(defgeneric (setf fd-non-blocking) (mode fd-mixin))

;;;; Bivalent Socket Gray Stream

(defclass dual-channel-gray-stream (trivial-gray-stream-mixin
                                    dual-channel-fd-mixin
                                    fundamental-binary-input-stream
                                    fundamental-binary-output-stream
                                    fundamental-character-input-stream
                                    fundamental-character-output-stream)
  ((external-format :initform :default :initarg :external-format
                    :reader external-format-of
                    :documentation "placehold")
   ;; Input buffer.
   (input-buffer :initform nil :type (or iobuf null)
                 :accessor input-buffer-of)
   ;; Output buffer.
   (output-buffer :initform nil :type (or iobuf null)
                  :accessor output-buffer-of)
   ;; Flag used by stream-force-output.
   (dirty :initform nil :type boolean :accessor dirtyp)
   ;; Last read char buffer index.
   (ibuf-unread-index :initform 0 :type buffer-index
                      :accessor ibuf-unread-index-of))
  (:documentation "placeholder"))

(defgeneric (setf external-format-of) (external-format stream)
  (:documentation "placeholder"))

(defgeneric drain-input-buffer (stream sequence &key start end)
  (:documentation ""))

(defgeneric input-buffer-size (stream)
  (:documentation ""))

(defgeneric input-buffer-empty-p (stream)
  (:documentation ""))

(defgeneric output-buffer-size (stream)
  (:documentation ""))

(defgeneric output-buffer-empty-p (stream)
  (:documentation ""))

(defclass dual-channel-single-fd-gray-stream
    (dual-channel-gray-stream dual-channel-single-fd-mixin)
  ()
  (:documentation "placeholder"))
