;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- I/O buffers.
;;;

(in-package :io.zeta-streams)

(declaim (optimize speed))

;;;; Foreign Buffers

(define-constant +default-iobuf-size+ 4096)

;;; almost 128 MB: large enough for a stream buffer,
;;; but small enough to fit into a fixnum
(deftype iobuf-index () '(unsigned-byte 27))
(deftype iobuf-length () '(integer 0 #.(expt 2 27)))

(deftype iobuf-data-vector () 'ub8-simple-vector)

(defstruct (iobuf (:constructor %make-iobuf (data)))
  (lock (bt:make-lock "IObuf lock") :read-only t)
  (data nil :type iobuf-data-vector :read-only t)
  (start 0 :type iobuf-index)
  (end 0 :type iobuf-index))

(defun make-iobuf-data-vector (size)
  (declare (type iobuf-index size))
  (make-array size :element-type 'ub8 :initial-element 0))

(defun make-iobuf (&optional size)
  (declare (type (or null iobuf-index) size))
  (%make-iobuf (make-iobuf-data-vector (or size +default-iobuf-size+))))

(defun iobuf-size (iobuf)
  (declare (type iobuf iobuf))
  (length (iobuf-data iobuf)))

(defun iobuf-available-octets (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-empty-p (iobuf)
  (declare (type iobuf iobuf))
  (zerop (iobuf-available-octets iobuf)))

(defun iobuf-full-p (iobuf)
  (declare (type iobuf iobuf))
  (= (iobuf-end iobuf)
     (iobuf-size iobuf)))

(defun iobuf-reset (iobuf)
  (declare (type iobuf iobuf))
  (setf (iobuf-start iobuf) 0
        (iobuf-end iobuf)   0))

(defun iobuf-next-data-zone (iobuf)
  (declare (type iobuf iobuf))
  (values (iobuf-data iobuf)
          (iobuf-start iobuf)
          (iobuf-end iobuf)))

(defun iobuf-next-empty-zone (iobuf)
  (declare (type iobuf iobuf))
  (values (iobuf-data iobuf)
          (iobuf-end iobuf)
          (iobuf-size iobuf)))


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
      (setf (iobuf-start iobuf) (1+ start)))))

(defun iobuf-push-octet (iobuf octet)
  (declare (type iobuf iobuf)
           (type ub8 octet))
  (let ((end (iobuf-end iobuf)))
    (prog1 (setf (bref iobuf end) octet)
      (setf (iobuf-end iobuf) (1+ end)))))

(defun replace-ub8 (destination source start1 end1 start2 end2)
  (declare (type ub8-simple-vector destination source)
           (type iobuf-index start1 start2 end1 end2))
  (let ((nbytes (min (- end1 start1)
                     (- end2 start2))))
    (replace destination source
             :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
    (values nbytes)))

(defun iobuf->vector (iobuf vector start end)
  (declare (type iobuf iobuf)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (when (iobuf-empty-p iobuf)
    (iobuf-reset iobuf))
  (multiple-value-bind (iobuf-data data-start data-end)
      (iobuf-next-data-zone iobuf)
    (let ((nbytes
           (replace-ub8 vector iobuf-data start end data-start data-end)))
      (setf (iobuf-start iobuf) (+ data-start nbytes))
      (values nbytes))))

(defun vector->iobuf (iobuf vector start end)
  (declare (type iobuf iobuf)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (when (iobuf-empty-p iobuf)
    (iobuf-reset iobuf))
  (multiple-value-bind (iobuf-data data-start data-end)
      (iobuf-next-empty-zone iobuf)
    (let ((nbytes
           (replace-ub8 iobuf-data vector data-start data-end start end)))
      (setf (iobuf-end iobuf) (+ data-end nbytes))
      (values nbytes))))
