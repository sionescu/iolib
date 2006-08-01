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
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 3)))

(in-package #:net.sockets)

(defclass dynamic-buffer ()
  ((contents :reader buffer-contents)
   (length   :initform 0      :reader buffer-length)
   (size     :initarg :size   :reader buffer-size))
  (:default-initargs :size +dns-datagram-size+))

(defmethod initialize-instance :after ((buffer dynamic-buffer)
                                       &key size)
  (setf (slot-value buffer 'contents)
        (make-array size :element-type '(unsigned-byte 8)
                    :adjustable t :fill-pointer 0)))

(defun ub16-to-vector (value)
  (vector (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun ub32-to-vector (value)
  (vector (ldb (byte 8 32) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defmethod output-vector :before ((buffer dynamic-buffer)
                                  vector)
  (with-slots (contents length size) buffer
    (let ((vector-length (length vector)))
      (when (< size (+ length vector-length))
        (let ((newsize (+ size vector-length 50)))
          (setf contents (adjust-array contents newsize))
          (setf size newsize))))))

(defmethod output-vector ((buffer dynamic-buffer)
                          vector)
  (with-slots (contents length) buffer
    (let ((vector-length (length vector)))
      (incf (fill-pointer contents) vector-length)
      (replace contents vector :start1 length)
      (incf length vector-length)))
  buffer)

(defmethod output-unsigned-8 ((buffer dynamic-buffer)
                               (value integer))
  (output-vector buffer (vector value)))

(defmethod output-unsigned-16 ((buffer dynamic-buffer)
                               (value integer))
  (output-vector buffer (ub16-to-vector value)))

(defmethod output-unsigned-32 ((buffer dynamic-buffer)
                               (value integer))
  (output-vector buffer (ub32-to-vector value)))

(defmethod output-string ((buffer dynamic-buffer)
                          (string simple-string))
  (output-unsigned-8 buffer (length string))
  (output-vector buffer (sb-ext:string-to-octets string)))

(defun domain-name-to-dns-format (domain-name)
  (let* ((octets (sb-ext:string-to-octets domain-name))
         (tmp-vec (make-array (1+ (length octets))
                              :element-type '(unsigned-byte 8))))
    (replace tmp-vec octets :start1 1)
    (let ((vector-length (length tmp-vec)))
      (loop
         :for start-off := 1 then (1+ end-off)
         :for end-off := (or (position (char-code #\.) tmp-vec :start start-off)
                             vector-length)
         :do (setf (aref tmp-vec (1- start-off)) (- end-off start-off))
         :when (>= end-off vector-length) :do (loop-finish)))
    tmp-vec))

(defmethod output-domain-name ((buffer dynamic-buffer)
                               (domain-name simple-string))
  (output-vector buffer (domain-name-to-dns-format domain-name)))
