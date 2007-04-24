;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

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

(in-package :net.sockets)

;;;
;;; Foreign Buffers
;;;

(iolib-utils:define-constant +bytes-per-iobuf+ (* 4 1024))

;; FIXME: make this right
;; probably not all SIMPLE-ARRAYs are admissible
;; on all implementations
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
    (setf (iobuf-data b) (cffi:foreign-alloc :uint8 :count size)
          (iobuf-size b) size)
    (values b)))

(defun free-iobuf (iobuf)
  (cffi:foreign-free (iobuf-data iobuf))
  (setf (iobuf-data iobuf) (cffi:null-pointer))
  (values iobuf))

(defun iobuf-length (iobuf)
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-start-pointer (iobuf)
  (cffi:inc-pointer (iobuf-data iobuf)
                    (iobuf-start iobuf)))

(defun iobuf-end-pointer (iobuf)
  (cffi:inc-pointer (iobuf-data iobuf)
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
  (et:memmove (iobuf-data iobuf)
              (cffi:inc-pointer (iobuf-data iobuf)
                                (iobuf-start iobuf))
              (iobuf-length iobuf)))

;; BREF, (SETF BREF) and BUFFER-COPY *DO NOT* check boundaries
;; that must be done by their callers
(defun bref (iobuf index)
  (declare (type iobuf iobuf)
           (type buffer-index index))
  (cffi:mem-aref (iobuf-data iobuf) :uint8 index))

(defun (setf bref) (octet iobuf index)
  (declare (type (unsigned-byte 8) octet)
           (type iobuf iobuf)
           (type buffer-index index))
  (setf (cffi:mem-aref (iobuf-data iobuf) :uint8 index) octet))

(defun iobuf-copy-from-lisp-array (src soff dst doff length)
  (declare (type compatible-lisp-array src)
           (type iobuf dst)
           (type buffer-index soff doff length))
  (let ((dst-ptr (iobuf-data dst)))
    (cffi:with-pointer-to-vector-data (src-ptr src)
      (et:memcpy (cffi:inc-pointer dst-ptr doff)
                 (cffi:inc-pointer src-ptr soff)
                 length))))

(defun iobuf-copy-into-lisp-array (src soff dst doff length)
  (declare (type iobuf src)
           (type compatible-lisp-array dst)
           (type buffer-index soff doff length))
  (let ((src-ptr (iobuf-data src)))
    (cffi:with-pointer-to-vector-data (dst-ptr dst)
      (et:memcpy (cffi:inc-pointer dst-ptr doff)
                 (cffi:inc-pointer src-ptr soff)
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

;;;
;;; Buffer Pool
;;;

(defstruct (iobuf-pool
             (:constructor make-iobuf-pool ())
             (:copier nil))
  (iobufs nil :type list)
  (count  0   :type unsigned-byte))

(defvar *available-iobufs* (make-iobuf-pool))

;; #-clisp
;; (defvar *iobuf-lock* (bordeaux-threads:make-lock "NET.SOCKETS STREAMS BUFFER POOL LOCK"))

;; FIXME: using a lock-free queue would be better
(defun next-available-iobuf ()
;;   #-clisp
;;   (bordeaux-threads:with-lock-held (*iobuf-lock*)
;;     (if (iobuf-pool-iobufs *available-iobufs*)
;;         (progn
;;           (pop (iobuf-pool-iobufs *available-iobufs*))
;;           (decf (iobuf-pool-count *available-iobufs*)))
;;         (%make-iobuf)))
;;   #+clisp
  (if (iobuf-pool-iobufs *available-iobufs*)
      (progn
        (pop (iobuf-pool-iobufs *available-iobufs*))
        (decf (iobuf-pool-count *available-iobufs*)))
      (%make-iobuf)))
