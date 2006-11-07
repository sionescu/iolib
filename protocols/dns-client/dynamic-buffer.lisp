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

(in-package #:net.sockets)

(deftype octet ()
  `(unsigned-byte 8))

(defclass dynamic-output-buffer ()
  ((sequence :initform nil    :reader buffer-sequence)
   (length   :initform 0      :reader buffer-length)
   (size     :initarg :size   :reader buffer-size))
  (:default-initargs :size +dns-datagram-size+))

(defmethod initialize-instance :after ((buffer dynamic-output-buffer)
                                       &key size)
  (setf (slot-value buffer 'sequence)
        (make-array size :element-type 'octet
                    :adjustable t :fill-pointer 0)))

(defun ub16-to-vector (value)
  (vector (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun ub32-to-vector (value)
  (vector (ldb (byte 8 32) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defgeneric write-vector (buffer vector))

(defmethod write-vector :before ((buffer dynamic-output-buffer)
                                 (vector array))
  (with-slots (sequence length size) buffer
    (let ((vector-length (length vector)))
      (when (< size (+ length vector-length))
        (let ((newsize (+ size vector-length 50)))
          (setf sequence (adjust-array sequence newsize))
          (setf size newsize))))))

(defmethod write-vector ((buffer dynamic-output-buffer)
                         (vector array))
  (with-slots (sequence length) buffer
    (let ((vector-length (length vector)))
      (incf (fill-pointer sequence) vector-length)
      (replace sequence vector :start1 length)
      (incf length vector-length)))
  buffer)

(defgeneric write-unsigned-8 (buffer vector))
(defmethod write-unsigned-8 ((buffer dynamic-output-buffer)
                             (value integer))
  (write-vector buffer (vector value)))

(defgeneric write-unsigned-16 (buffer vector))
(defmethod write-unsigned-16 ((buffer dynamic-output-buffer)
                              (value integer))
  (write-vector buffer (ub16-to-vector value)))

(defgeneric write-unsigned-32 (buffer vector))
(defmethod write-unsigned-32 ((buffer dynamic-output-buffer)
                              (value integer))
  (write-vector buffer (ub32-to-vector value)))

(defmacro with-output-buffer (var &body body)
  `(let ((,var (make-instance 'dynamic-output-buffer)))
     ,@body
     ,var))


(defclass dynamic-input-buffer ()
  ((sequence :initform nil :initarg :sequence :reader buffer-sequence)
   (position :initform 0   :reader buffer-position)
   (size     :reader buffer-size)))

(defmethod initialize-instance :after ((buffer dynamic-input-buffer) &key size)
  (with-slots (sequence (seq-size size)) buffer
    (setf seq-size (or size (length sequence)))
    (cond
      ((null sequence)
       (setf sequence (make-array 0 :element-type 'octet :adjustable t
                                  :initial-contents sequence)))
      ((not (and (adjustable-array-p sequence)
                 (typep sequence '(vector octet))))
       (setf sequence (make-array seq-size
                                  :element-type 'octet :adjustable t
                                  :displaced-to sequence))))))

(define-condition input-buffer-error (error) ())

(define-condition input-buffer-scarcity (input-buffer-error)
  ((bytes-requested :initarg :requested :reader bytes-requested)
   (bytes-remaining :initarg :remaining :reader bytes-remaining))
  (:documentation "Signals that an INPUT-BUFFER contains less unread bytes than requested."))

(define-condition input-buffer-eof (input-buffer-scarcity) ()
  (:documentation "Signals that an INPUT-BUFFER contains no more unread bytes."))

(define-condition input-buffer-index-out-of-bounds (input-buffer-error) ()
  (:documentation "Signals that BUFFER-SEEK on an INPUT-BUFFER was passed an invalid offset."))

(defgeneric buffer-seek (buffer offset))
(defmethod buffer-seek ((buffer dynamic-input-buffer) offset)
  (check-type offset unsigned-byte "a non-negative value")
  (with-slots (sequence size position) buffer
    (if (> offset (1- size))
        (error 'input-buffer-index-out-of-bounds)
        (setf position offset))))

(defgeneric buffer-append (buffer vector))
(defmethod buffer-append ((buffer dynamic-input-buffer)
                          vector)
  (with-slots (sequence size) buffer
    (when (plusp (length vector))
      (let ((oldsize size)
            (newsize (+ (length sequence)
                        (length vector))))
        (setf sequence (adjust-array sequence newsize))
        (replace sequence vector :start1 oldsize)
        (setf size newsize)))))

(defgeneric bytes-unread (buffer))
(defmethod bytes-unread ((buffer dynamic-input-buffer))
  (with-slots (position size) buffer
    (- size position)))

(defgeneric check-if-enough-bytes (buffer length &key check-all))
(defmethod check-if-enough-bytes ((buffer dynamic-input-buffer)
                                  length &key (check-all t))
  (let ((bytes-unread (bytes-unread buffer)))
    (cond
      ((and (zerop bytes-unread)
            (plusp length))
       (error 'input-buffer-eof
              :requested length
              :remaining bytes-unread))
      ((and check-all
            (< bytes-unread length))
       (error 'input-buffer-scarcity
              :requested length
              :remaining bytes-unread)))
    t))

(defun read-ub16-from-vector (vector position)
  (+ (ash (aref vector position) 8)
     (aref vector (1+ position))))

(defun read-ub32-from-vector (vector position)
  (+ (ash (aref vector position) 24)
     (ash (aref vector (1+ position)) 16)
     (ash (aref vector (+ position 2)) 8)
     (aref vector (+ position 3))))

(defgeneric read-vector (buffer length &key read-all))
(defmethod read-vector ((buffer dynamic-input-buffer)
                        length &key (read-all t))
  (let* ((bytes-to-read
          (min (bytes-unread buffer) length))
         (newvector
          (make-array bytes-to-read :element-type 'octet)))
    (check-if-enough-bytes buffer length :check-all read-all)
    (with-slots (sequence position) buffer
      (replace newvector sequence :start2 position)
      (incf position bytes-to-read))
    newvector))

(defgeneric read-unsigned-8 (buffer))
(defmethod read-unsigned-8 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 1)
  (with-slots (sequence position) buffer
    (prog1
        (aref sequence position)
      (incf position))))

(defgeneric read-unsigned-16 (buffer))
(defmethod read-unsigned-16 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 2)
  (with-slots (sequence position) buffer
    (prog1
        (read-ub16-from-vector sequence position)
      (incf position 2))))

(defgeneric read-unsigned-32 (buffer))
(defmethod read-unsigned-32 ((buffer dynamic-input-buffer))
  (check-if-enough-bytes buffer 4)
  (with-slots (sequence position) buffer
    (prog1
        (read-ub32-from-vector sequence position)
      (incf position 4))))

(defmacro with-input-buffer ((var) &body body)
  `(let ((,var (make-instance 'dynamic-input-buffer)))
     ,@body
     ,var))
