;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign memory buffers.
;;;

(in-package :io.zeta-streams)

(declaim (optimize speed))

;;;; Foreign Buffers

(define-constant +default-iobuf-size+ 4096)

;;; almost 128 MB: large enough for a stream buffer,
;;; but small enough to fit into a fixnum
(deftype iobuf-index () '(unsigned-byte 27))
(deftype iobuf-length () '(integer 0 #.(expt 2 27)))

(deftype iobuf-buffer () 'ub8-sarray)

(defparameter *empty-array* (make-array 0 :element-type 'ub8))

(defstruct (iobuf (:constructor %make-iobuf ()))
  (data *empty-array* :type iobuf-buffer)
  (start 0 :type iobuf-index)
  (end 0 :type iobuf-index))

(defun make-iobuf (&optional size)
  (declare (type (or null iobuf-index) size))
  (let ((b (%make-iobuf)))
    (setf (iobuf-data b) (make-array (or size +default-iobuf-size+)
                                     :element-type 'ub8
                                     :initial-element 0))
    (values b)))

(defun iobuf-size (iobuf)
  (declare (type iobuf iobuf))
  (length (iobuf-data iobuf)))

(defun iobuf-available-octets (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-empty-p (iobuf)
  (declare (type iobuf iobuf))
  (= (iobuf-start iobuf)
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
           (type ub8 octet))
  (let ((end (iobuf-end iobuf)))
    (prog1 (setf (bref iobuf end) octet)
      (incf (iobuf-end iobuf)))))

(defun replace-ub8 (destination source start1 end1 start2 end2)
  (declare (type iobuf-buffer destination source)
           (type iobuf-index start1 start2 end1 end2))
  (let ((nbytes (min (- end1 start1)
                     (- end2 start2))))
    (replace destination source
             :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
    (values destination nbytes)))

(defun iobuf->array (array iobuf start end)
  (declare (type iobuf-buffer array)
           (type iobuf iobuf)
           (type iobuf-index start end))
  (let ((nbytes
         (nth-value 1 (replace-ub8 array (iobuf-data iobuf)
                                   start end
                                   (iobuf-start iobuf)
                                   (iobuf-end iobuf)))))
    (incf (iobuf-start iobuf) nbytes)
    (values nbytes)))

(defun array->iobuf (iobuf array start end)
  (declare (type iobuf-buffer array)
           (type iobuf iobuf)
           (type iobuf-index start end))
  (let ((nbytes
         (nth-value 1 (replace-ub8 (iobuf-data iobuf) array
                                   (iobuf-start iobuf)
                                   (iobuf-end iobuf)
                                   start end))))
    (incf (iobuf-end iobuf) nbytes)
    (values nbytes)))
