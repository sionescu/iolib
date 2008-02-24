;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; buffer.lisp --- Foreign memory buffers.
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

;;;; Foreign Buffers

(define-constant +bytes-per-iobuf+ 4096)

;;; FIXME: make this right
;;; probably not all SIMPLE-ARRAYs are admissible
;;; on all implementations
(deftype compatible-lisp-array ()
  '(simple-array * (*)))

(declaim (inline allocate-iobuf iobuf-length iobuf-start-pointer
                 iobuf-end-pointer iobuf-end-space-length
                 iobuf-empty-p iobuf-full-p
                 iobuf-reset iobuf-copy-data-to-start
                 bref (setf bref) iobuf-copy
                 iobuf-pop-octet iobuf-push-octet))

(defun make-iobuf (&optional (size +bytes-per-iobuf+))
  (let ((b (%make-iobuf)))
    (setf (iobuf-data b) (foreign-alloc :uint8 :count size)
          (iobuf-size b) size)
    (values b)))

(defun iobuf-length (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-start-pointer (iobuf data-pointer)
  (declare (type iobuf iobuf)
           (type cffi:foreign-pointer))
  (inc-pointer data-pointer (iobuf-start iobuf)))

(defun iobuf-end-pointer (iobuf data-pointer)
  (declare (type iobuf iobuf)
           (type cffi:foreign-pointer))
  (inc-pointer data-pointer (iobuf-end iobuf)))

(defun iobuf-empty-p (iobuf)
  (declare (type iobuf iobuf))
  (= (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-full-p (iobuf)
  (declare (type iobuf iobuf))
  (= (iobuf-end iobuf)
     (iobuf-size iobuf)))

(defun iobuf-end-space-length (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-size iobuf)
     (iobuf-end iobuf)))

(defun iobuf-reset (iobuf)
  (declare (type iobuf iobuf))
  (setf (iobuf-start iobuf) 0
        (iobuf-end iobuf)   0))

(defun iobuf-copy-data-to-start (iobuf)
  (declare (type iobuf iobuf))
  (cffi:with-pointer-to-vector-data (ptr (iobuf-data iobuf))
    (nix:memmove ptr (cffi:inc-pointer ptr (iobuf-start iobuf))
                 (iobuf-length iobuf)))
  (setf (iobuf-end iobuf) (iobuf-length iobuf))
  (setf (iobuf-start iobuf) 0))

;;; BREF, (SETF BREF) and BUFFER-COPY *DO NOT* check boundaries
;;; that must be done by their callers
(defun bref (iobuf index)
  (declare (type iobuf iobuf)
           (type iobuf-index index))
  (aref (iobuf-data iobuf) index))

(defun (setf bref) (octet iobuf index)
  (declare (type ub8 octet)
           (type iobuf iobuf)
           (type iobuf-index index))
  (setf (aref (iobuf-data iobuf) index) octet))

(defun iobuf-pop-octet (iobuf)
  (declare (type iobuf iobuf))
  (let ((start (iobuf-start iobuf)))
    (prog1 (bref iobuf start)
      (incf (iobuf-start iobuf)))))

(defun iobuf-push-octet (iobuf octet)
  (declare (type iobuf iobuf)
           (type (unsigned-byte 8) octet))
  (let ((end (iobuf-end iobuf)))
    (prog1 (setf (bref iobuf end) octet)
      (incf (iobuf-end iobuf)))))
