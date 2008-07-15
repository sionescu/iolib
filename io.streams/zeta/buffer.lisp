;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device buffers.
;;;

(in-package :io.zeta-streams)

(defclass filter (dual-channel-device) ())

(defclass device-buffer (filter)
  ((input-buffer :initarg :input-buffer :accessor input-buffer-of)
   (output-buffer :initarg :output-buffer :accessor output-buffer-of)))

(defmethod initialize-instance :after ((filter filter) &key
                                       input-buffer-size output-buffer-size)
  (if (input-buffer-of filter)
      (check-type (input-buffer-of filter) iobuf)
      (setf (input-buffer-of filter) (make-iobuf input-buffer-size)))
  (if (output-buffer-of filter)
      (check-type (output-buffer-of filter) iobuf)
      (setf (output-buffer-of filter) (make-iobuf output-buffer-size))))


;;;-----------------------------------------------------------------------------
;;; Buffered DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read ((device device-buffer) buffer start end &optional (timeout nil timeoutp))
  (when (= start end) (return-from device-read 0))
  (let* ((timeout (if timeoutp timeout (input-timeout-of (input-handle-of device))))
         (nbytes (read-octets/buffered (input-handle-of device) buffer start end timeout)))
    (cond
      ((eql :eof nbytes) (return-from device-read :eof))
      ((plusp nbytes) (incf (device-position device) nbytes)))
    (values nbytes)))

(defun fill-input-buffer (input-handle input-buffer timeout)
  (declare (type device input-handle)
           (type iobuf input-buffer)
           (type device-timeout timeout))
  (device-read input-handle (iobuf-data input-buffer)
               (iobuf-end input-buffer) (iobuf-size input-buffer)
               timeout))

(defun read-octets/buffered (device buffer start end timeout)
  (declare (type device-buffer device)
           (type iobuf-buffer buffer)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-accessors ((input-handle input-handle-of)
                   (input-buffer input-buffer-of))
      device
    (cond
      ((iobuf-empty-p input-buffer)
       (iobuf-reset input-buffer)
       (let ((nbytes (fill-input-buffer input-handle input-buffer timeout)))
         (if (iobuf-empty-p input-buffer)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->array buffer input-buffer start end))))
      (t
       (iobuf->array buffer input-buffer start end)))))


;;;-----------------------------------------------------------------------------
;;; Buffered DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write ((device device-buffer) buffer start end &optional (timeout nil timeoutp))
  (when (= start end) (return-from device-write 0))
  (let* ((timeout (if timeoutp timeout (output-timeout-of (output-handle-of device))))
         (nbytes (write-octets/buffered (output-handle-of device) buffer start end timeout)))
    (cond
      ((eql :eof nbytes) (return-from device-write :eof))
      ((plusp nbytes) (incf (device-position device) nbytes)))
    (values nbytes)))
