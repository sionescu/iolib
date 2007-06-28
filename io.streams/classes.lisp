;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :io.streams)

;;;;;;;;;;;;;
;;;       ;;;
;;; Types ;;;
;;;       ;;;
;;;;;;;;;;;;;

(deftype ub8  () `(unsigned-byte 8))
(deftype ub16 () `(unsigned-byte 16))
(deftype ub32 () `(unsigned-byte 32))
(deftype sb8  () `(signed-byte 8))
(deftype sb16 () `(signed-byte 16))
(deftype sb32 () `(signed-byte 32))

(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))
(deftype ub8-vector ()
  '(vector ub8))
(deftype ub16-sarray (&optional (size '*))
  `(simple-array ub16 (,size)))


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Socket buffers ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(deftype stream-buffer ()
  'foreign-pointer)

(deftype buffer-index ()
  '(unsigned-byte 24))

(defstruct (iobuf
             (:constructor %make-iobuf ()))
  (data (null-pointer) :type stream-buffer)
  (size 0 :type buffer-index)
  (start 0 :type buffer-index)
  (end 0 :type buffer-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        ;;;
;;; File-Descriptor Mixins ;;;
;;;                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stream-position ()
  '(unsigned-byte 64))

(defclass dual-channel-fd-mixin ()
  ((input-fd  :initform nil :accessor input-fd-of)
   (output-fd :initform nil :accessor output-fd-of)))

(defgeneric input-fd-non-blocking (socket))
(defgeneric (setf input-fd-non-blocking) (mode fd-mixin))

(defgeneric output-fd-non-blocking (socket))
(defgeneric (setf output-fd-non-blocking) (mode fd-mixin))


(defclass dual-channel-single-fd-mixin (dual-channel-fd-mixin) ())

(defgeneric fd-of (stream)
  (:method ((stream dual-channel-single-fd-mixin))
    (with-accessors ((fd-in  input-fd-of)
                     (fd-out output-fd-of)) stream
      (assert (eql fd-in fd-out))
      (values fd-in))))
(defgeneric (setf fd-of) (fd stream)
  (:method (fd (stream dual-channel-single-fd-mixin))
    (with-accessors ((fd-in  input-fd-of)
                     (fd-out output-fd-of)) stream
      (assert (eql fd-in fd-out))
      (setf fd-in fd fd-out fd)
      (values fd-in))))

(defgeneric fd-non-blocking (fd-mixin))
(defgeneric (setf fd-non-blocking) (mode fd-mixin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             ;;;
;;; Bivalent socket Gray stream ;;;
;;;                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dual-channel-gray-stream (dual-channel-fd-mixin
                                    fundamental-binary-input-stream
                                    fundamental-binary-output-stream
                                    fundamental-character-input-stream
                                    fundamental-character-output-stream)
  ((external-format :initarg :external-format
                    :reader external-format-of)
   ;; Input buffer.
   (input-buffer :initform nil :type (or iobuf null)
                 :accessor input-buffer-of)
   ;; Output buffer.
   (output-buffer :initform nil :type (or iobuf null)
                  :accessor output-buffer-of)
   ;; Flag used by stream-force-output
   (must-flush-output :initform nil :type boolean
                      :accessor must-flush-output-p)
   ;; Last read char buffer index
   (ibuf-unread-index :initform 0 :type buffer-index
                      :accessor ibuf-unread-index-of))
  (:default-initargs :external-format :default))
