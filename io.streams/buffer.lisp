;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; buffer.lisp --- Foreign memory buffers.
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

(in-package :io.streams)

;;;; Foreign Buffers

(define-constant +bytes-per-iobuf+ (* 4 1024))

;;; FIXME: make this right
;;; probably not all SIMPLE-ARRAYs are admissible
;;; on all implementations
(deftype compatible-lisp-array ()
  '(simple-array * (*)))

(declaim (inline allocate-iobuf free-iobuf
                 iobuf-length iobuf-start-pointer
                 iobuf-end-pointer iobuf-end-space-length
                 iobuf-empty-p iobuf-full-p
                 iobuf-reset iobuf-copy-data-to-start
                 bref (setf bref) iobuf-copy
                 iobuf-pop-octet iobuf-push-octet))

(defun allocate-iobuf (&optional (size +bytes-per-iobuf+))
  (let ((b (%make-iobuf)))
    (setf (iobuf-data b) (foreign-alloc :uint8 :count size)
          (iobuf-size b) size)
    (values b)))

(defun free-iobuf (iobuf)
  (foreign-free (iobuf-data iobuf))
  (setf (iobuf-data iobuf) (null-pointer))
  (values iobuf))

(defun iobuf-length (iobuf)
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-start-pointer (iobuf)
  (inc-pointer (iobuf-data iobuf)
               (iobuf-start iobuf)))

(defun iobuf-end-pointer (iobuf)
  (inc-pointer (iobuf-data iobuf)
               (iobuf-end iobuf)))

(defun iobuf-empty-p (iobuf)
  (= (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-full-p (iobuf)
  (= (iobuf-end iobuf)
     (iobuf-size iobuf)))

(defun iobuf-end-space-length (iobuf)
  (- (iobuf-size iobuf)
     (iobuf-end iobuf)))

(defun iobuf-reset (iobuf)
  (setf (iobuf-start iobuf) 0
        (iobuf-end iobuf)   0))

(defun iobuf-copy-data-to-start (iobuf)
  (declare (type iobuf iobuf))
  (nix:memmove
   (iobuf-data iobuf)
   (inc-pointer (iobuf-data iobuf)
                (iobuf-start iobuf))
   (iobuf-length iobuf))
  (setf (iobuf-end iobuf) (iobuf-length iobuf))
  (setf (iobuf-start iobuf) 0))

;;; BREF, (SETF BREF) and BUFFER-COPY *DO NOT* check boundaries
;;; that must be done by their callers
(defun bref (iobuf index)
  (declare (type iobuf iobuf)
           (type buffer-index index))
  (mem-aref (iobuf-data iobuf) :uint8 index))

(defun (setf bref) (octet iobuf index)
  (declare (type (unsigned-byte 8) octet)
           (type iobuf iobuf)
           (type buffer-index index))
  (setf (mem-aref (iobuf-data iobuf) :uint8 index) octet))

(defun iobuf-copy-from-lisp-array (src soff dst doff length)
  (declare (type compatible-lisp-array src)
           (type iobuf dst)
           (type buffer-index soff doff length))
  (let ((dst-ptr (iobuf-data dst)))
    (with-pointer-to-vector-data (src-ptr src)
      (nix:memcpy
       (inc-pointer dst-ptr doff)
       (inc-pointer src-ptr soff)
       length))))

(defun iobuf-copy-into-lisp-array (src soff dst doff length)
  (declare (type iobuf src)
           (type compatible-lisp-array dst)
           (type buffer-index soff doff length))
  (let ((src-ptr (iobuf-data src)))
    (with-pointer-to-vector-data (dst-ptr dst)
      (nix:memcpy
       (inc-pointer dst-ptr doff)
       (inc-pointer src-ptr soff)
       length))))

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
