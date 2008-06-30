;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign memory buffers.
;;;

(in-package :io.zeta-streams)

;;;; Foreign Buffers

(define-constant +bytes-per-iobuf+ 4096)

(defun make-iobuf (&optional (size +bytes-per-iobuf+))
  (let ((b (%make-iobuf)))
    (setf (iobuf-data b) (make-array size :element-type 'ub8)
          (iobuf-size b) size)
    (values b)))

(defun iobuf-length (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

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


;;;
;;; UNSAFE functions which *DO NOT* check boundaries
;;; that must be done by their callers
;;;

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
