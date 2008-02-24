;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; classes.lisp --- Zeta streams classes.
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

(in-package :io.zeta-streams)

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

;;;; Buffers

;;; almost 128 MB: large enough for a stream buffer,
;;; but small enough to fit into a fixnum
(deftype iobuf-index () '(unsigned-byte 27))
(deftype iobuf-length () '(integer 0 #.(expt 2 27)))

(deftype iobuf-buffer () '(simple-array ub8 (*)))

(defstruct (iobuf (:constructor %make-iobuf ()))
  (data (make-array 0 :element-type 'ub8)
        :type iobuf-buffer)
  (size 0 :type iobuf-length)
  (start 0 :type iobuf-index)
  (end 0 :type iobuf-index))

;;;; Stream Classes

(deftype stream-position () '(unsigned-byte 64))

(defclass zeta-stream () ())

(defclass single-channel-zeta-stream (zeta-stream)
  ())

(defclass dual-channel-zeta-stream (zeta-stream)
  ())

(defclass memory-buffer-zeta-stream (zeta-stream)
  ())
