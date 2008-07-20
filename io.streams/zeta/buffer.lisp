;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device buffers.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Buffer Classes and Types
;;;-----------------------------------------------------------------------------

(defclass buffer ()
  ((synchronized :initarg :synchronized :reader synchronizedp)
   (input-iobuf :initarg :input-buffer :accessor input-iobuf-of)
   (output-iobuf :initarg :output-buffer :accessor output-iobuf-of))
  (:default-initargs :synchronized nil))

(defclass single-channel-buffer (single-channel-device buffer)
  ((last-io-op :initform nil :accessor last-io-op-of)))

(defclass dual-channel-buffer (dual-channel-device buffer) ())


;;;-----------------------------------------------------------------------------
;;; Buffer Generic Functions
;;;-----------------------------------------------------------------------------

(defgeneric buffer-clear-input (buffer))

(defgeneric buffer-clear-output (buffer))

(defgeneric buffer-fill-input (buffer &optional timeout))

(defgeneric buffer-flush-output (buffer &optional timeout))

;;; Internal functions

(defgeneric buffer-read-octets (buffer vector start end timeout))

(defgeneric buffer-write-octets (buffer vector start end timeout))

(defgeneric %buffer-clear-input (buffer))

(defgeneric %buffer-fill-input (buffer timeout))

(defgeneric %buffer-flush-output (buffer timeout))


;;;-----------------------------------------------------------------------------
;;; Helper macros
;;;-----------------------------------------------------------------------------

(defmacro with-synchronized-buffer ((buffer &optional direction)
                                                 &body body)
  (with-gensyms (body-fun)
    (labels ((make-locks (body direction)
               (ecase direction
                 (:input
                  `(bt:with-lock-held ((iobuf-lock (input-iobuf-of ,buffer)))
                     ,body))
                 (:output
                  `(bt:with-lock-held ((iobuf-lock (output-iobuf-of ,buffer)))
                     ,body))
                 (:both
                  (make-locks (make-locks body :output) :input)))))
      `(flet ((,body-fun () ,@body))
         (if (synchronizedp ,buffer)
             ,(make-locks `(,body-fun) direction)
             (,body-fun))))))


;;;-----------------------------------------------------------------------------
;;; Buffer Constructors
;;;-----------------------------------------------------------------------------

(defmethod initialize-instance :after
    ((device single-channel-buffer) &key buffer buffer-size)
  (with-accessors ((input-iobuf input-iobuf-of)
                   (output-iobuf output-iobuf-of))
      device
    (check-type buffer (or null iobuf))
    (setf input-iobuf (or buffer (make-iobuf buffer-size))
          output-iobuf input-iobuf)))

(defmethod initialize-instance :after
    ((device dual-channel-buffer) &key input-buffer output-buffer
     input-buffer-size output-buffer-size)
  (with-accessors ((input-iobuf input-iobuf-of)
                   (output-iobuf output-iobuf-of))
      device
    (check-type input-buffer (or null iobuf))
    (check-type output-buffer (or null iobuf))
    (setf input-iobuf (or input-buffer (make-iobuf input-buffer-size)))
    (setf output-iobuf (or output-buffer (make-iobuf output-buffer-size)))))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-CLOSE
;;;-----------------------------------------------------------------------------

(defmethod device-close ((buffer single-channel-buffer) &optional abort)
  (with-accessors ((handle input-handle-of))
      buffer
    (with-synchronized-buffer (buffer :input)
      (unless (or abort (eql :read (last-io-op-of buffer)))
        (%buffer-flush-output buffer 0))
      (device-close handle)))
  (values buffer))

(defmethod device-close ((buffer buffer) &optional abort)
  (with-accessors ((input-handle input-handle-of buffer)
                   (output-handle output-handle-of buffer))
      buffer
    (with-synchronized-buffer (buffer :both)
      (unless abort
        (%buffer-flush-output buffer 0))
      (device-close input-handle)
      (device-close output-handle)))
  (values buffer))

;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read ((buffer single-channel-buffer) vector start end
                        &optional timeout)
  (when (= start end) (return-from device-read 0))
  (with-synchronized-buffer (buffer :input)
    ;; If the previous operation was a write, try to flush the output buffer.
    ;; If the buffer couldn't be flushed entirely, signal an error
    (synchronize-input buffer)
    (buffer-read-octets buffer buffer start end timeout)))

(defmethod device-read ((buffer dual-channel-buffer) vector start end
                        &optional timeout)
  (when (= start end) (return-from device-read 0))
  (with-synchronized-buffer (buffer :input)
    (buffer-read-octets buffer buffer start end timeout)))

(defmethod buffer-read-octets ((buffer buffer) vector start end timeout)
  (with-accessors ((input-handle input-handle-of)
                   (input-iobuf input-iobuf-of)
                   (output-handle output-handle-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (cond
      ((iobuf-empty-p input-iobuf)
       (let ((nbytes
              (%buffer-fill-input buffer timeout)))
         (if (iobuf-empty-p input-iobuf)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->vector input-iobuf vector start end))))
      (t
       (iobuf->vector input-iobuf vector start end)))))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write ((buffer single-channel-buffer) vector start end
                         &optional timeout)
  (when (= start end) (return-from device-write 0))
  (with-synchronized-buffer (buffer :input)
    ;; If the previous operation was a read, flush the read buffer
    ;; and reposition the file offset accordingly
    (%buffer-clear-input buffer)
    (buffer-write-octets buffer vector start end timeout)))

(defmethod device-write ((buffer dual-channel-buffer) vector start end
                         &optional timeout)
  (when (= start end) (return-from device-write 0))
  (with-synchronized-buffer (buffer :output)
    (buffer-write-octets buffer vector start end timeout)))

(defmethod buffer-write-octets ((buffer buffer) vector start end timeout)
  (with-accessors ((output-handle output-handle-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (prog1
        (vector->iobuf output-iobuf vector start end)
      (setf (last-io-op-of buffer) :write)
      (when (iobuf-full-p output-iobuf)
        (%buffer-flush-output buffer timeout)))))


;;;-----------------------------------------------------------------------------
;;; Buffer DEVICE-POSITION
;;;-----------------------------------------------------------------------------

(defmethod device-position ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (%buffer-position buffer)))

(defun %buffer-position (buffer)
  (let ((position (device-position (input-handle-of buffer))))
    (ecase (last-io-op-of buffer)
      (:read
       (- position (iobuf-available-octets (input-iobuf-of buffer))))
      (:write
       (+ position (iobuf-available-octets (output-iobuf-of buffer)))))))

(defmethod (setf device-position) (position (buffer single-channel-buffer) &key (from :start))
  (setf (%buffer-position buffer from) position))

(defun (setf %buffer-position) (position buffer from)
  (setf (device-position (input-handle-of buffer) :from from) position))


;;;-----------------------------------------------------------------------------
;;; BUFFER CLEAR-INPUT
;;;-----------------------------------------------------------------------------

(defmethod buffer-clear-input ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (%buffer-clear-input buffer)))

(defmethod %buffer-clear-input ((buffer single-channel-buffer))
  (when (eql :read (last-io-op-of buffer))
    (let ((nbytes (iobuf-available-octets (input-iobuf-of buffer))))
      (unless (zerop nbytes)
        (setf (%buffer-position buffer :current) (- nbytes)))
      (iobuf-reset (input-iobuf-of buffer)))))

(defmethod buffer-clear-input ((buffer buffer))
  (with-synchronized-buffer (buffer :input)
    (%buffer-clear-input buffer)))

(defmethod %buffer-clear-input ((buffer dual-channel-buffer))
  (iobuf-reset (input-iobuf-of buffer)))


;;;-----------------------------------------------------------------------------
;;; BUFFER CLEAR-OUTPUT
;;;-----------------------------------------------------------------------------

(defmethod buffer-clear-output ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (when (eql :write (last-io-op-of buffer))
      (iobuf-reset (output-iobuf-of buffer)))))

(defmethod buffer-clear-output ((buffer dual-channel-buffer))
  (with-synchronized-buffer (buffer :output)
    (iobuf-reset (output-iobuf-of buffer))))


;;;-----------------------------------------------------------------------------
;;; BUFFER FILL-INPUT
;;;-----------------------------------------------------------------------------

(defmethod buffer-fill-input ((buffer single-channel-buffer) &optional timeout)
  (with-synchronized-buffer (buffer :input)
    ;; If the previous operation was a write, try to flush the output buffer.
    ;; If the buffer couldn't be flushed entirely, signal an error
    (synchronize-input buffer)
    (%buffer-fill-input buffer timeout)))

(defun synchronize-input (buffer)
  (when (and (eql :write (last-io-op-of buffer))
             (plusp (%buffer-flush-output buffer 0)))
    ;; FIXME: What do we do now ???
    (error "Could not flush the entire write buffer !"))
  (iobuf-reset (output-iobuf-of buffer)))

(defmethod buffer-fill-input ((buffer dual-channel-buffer) &optional timeout)
  (with-synchronized-buffer (buffer :input)
    (%buffer-fill-input buffer timeout)))

(defmethod %buffer-fill-input ((buffer buffer) timeout)
  (with-accessors ((input-handle input-handle-of)
                   (input-iobuf input-iobuf-of))
      buffer
    (multiple-value-bind (data start end)
        (iobuf-next-empty-zone input-iobuf)
      (let ((nbytes
             (device-read input-handle data start end timeout)))
        (setf (iobuf-end input-iobuf) (+ start nbytes))
        (setf (last-io-op-of buffer) :read)
        (values nbytes)))))


;;;-----------------------------------------------------------------------------
;;; BUFFER FLUSH-OUTPUT
;;;-----------------------------------------------------------------------------

(defmethod buffer-flush-output ((buffer single-channel-buffer) &optional timeout)
  (with-synchronized-buffer (buffer :input)
    (when (eql :write (last-io-op-of buffer))
      (%buffer-flush-output buffer timeout))))

(defmethod buffer-flush-output ((buffer dual-channel-buffer) &optional timeout)
  (with-synchronized-buffer (buffer :output)
    (%buffer-flush-output buffer timeout)))

(defmethod %buffer-flush-output ((buffer dual-channel-buffer) timeout)
  (with-accessors ((output-handle output-handle-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (multiple-value-bind (data start end)
        (iobuf-next-data-zone output-iobuf)
      (let ((nbytes
             (device-write output-handle data start end timeout)))
        (setf (iobuf-start output-iobuf) (+ start nbytes))
        (setf (last-io-op-of buffer) :write)
        (iobuf-available-octets output-iobuf)))))
