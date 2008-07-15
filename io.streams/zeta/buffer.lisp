;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device buffers.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Buffer Classes and Types
;;;-----------------------------------------------------------------------------

(defclass buffer (device)
  ((single-channel-p :initarg :single-channel :accessor single-channel-buffer-p)
   (input-buffer :initarg :input-buffer :accessor input-buffer-of)
   (output-buffer :initarg :output-buffer :accessor output-buffer-of))
  (:default-initargs :single-channel nil))


;;;-----------------------------------------------------------------------------
;;; Buffer Constructors
;;;-----------------------------------------------------------------------------

(defmethod initialize-instance :after ((buffer buffer) &key single-channel
                                       input-buffer-size output-buffer-size)
  (with-accessors ((input-buffer input-buffer-of)
                   (output-buffer output-buffer-of))
      buffer
    (if input-buffer
        (check-type input-buffer iobuf)
        (setf input-buffer (make-iobuf input-buffer-size)))
    (if single-channel
        (setf output-buffer input-buffer)
        (cond
          (output-buffer
           (check-type output-buffer iobuf)
           (assert (not (eq input-buffer output-buffer))))
          (t (setf output-buffer (make-iobuf output-buffer-size)))))))


;;;-----------------------------------------------------------------------------
;;; Buffer Generic Functions
;;;-----------------------------------------------------------------------------

(defgeneric buffer-clear-input (buffer))

(defgeneric buffer-clear-output (buffer))

(defgeneric buffer-flush-output (buffer &optional timeout))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read ((device buffer) buffer start end &optional timeout)
  (when (= start end) (return-from device-read 0))
  (read-octets/buffered device buffer start end timeout))

(defun read-octets/buffered (buffer vector start end timeout)
  (declare (type buffer buffer)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-accessors ((input-handle input-handle-of)
                   (input-buffer input-buffer-of))
      buffer
    (cond
      ((iobuf-empty-p input-buffer)
       (let ((nbytes (fill-input-buffer input-handle input-buffer timeout)))
         (if (iobuf-empty-p input-buffer)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->vector input-buffer vector start end))))
      (t
       (iobuf->vector input-buffer vector start end)))))

(defun fill-input-buffer (input-handle input-buffer timeout)
  (multiple-value-bind (data start end)
      (iobuf-next-empty-zone input-buffer)
    (device-read input-handle data start end timeout)))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write ((device buffer) buffer start end &optional timeout)
  (when (= start end) (return-from device-write 0))
  (write-octets/buffered device buffer start end timeout))

(defun write-octets/buffered (buffer vector start end timeout)
  (declare (type buffer buffer)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-accessors ((output-handle output-handle-of)
                   (output-buffer output-buffer-of))
      buffer
    (vector->iobuf output-buffer vector start end)
    (when (iobuf-full-p output-buffer)
      (flush-output-buffer output-handle output-buffer timeout))))

(defun flush-output-buffer (output-handle output-buffer timeout)
  (multiple-value-bind (data start end)
      (iobuf-next-data-zone output-buffer)
    (device-write output-handle data start end timeout)))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-POSITION
;;;-----------------------------------------------------------------------------

(defmethod device-position ((device buffer))
  (when-let ((handle-position
              (device-position (input-handle-of device))))
    (+ handle-position (iobuf-available-octets (input-buffer-of device)))))

(defmethod (setf device-position) (position (device buffer) &key (from :start))
  (setf (device-position device :from from) position))


;;;-----------------------------------------------------------------------------
;;; Buffer cleaning
;;;-----------------------------------------------------------------------------

(defmethod buffer-clear-input ((buffer buffer))
  (iobuf-reset (input-buffer-of buffer)))

(defmethod buffer-clear-output ((buffer buffer))
  (iobuf-reset (output-buffer-of buffer)))

(defmethod buffer-flush-output ((buffer buffer) &optional timeout)
  (with-accessors ((output-handle output-handle-of)
                   (output-buffer output-buffer-of))
      buffer
    (flush-output-buffer output-handle output-buffer timeout)
    (iobuf-available-octets output-buffer)))
