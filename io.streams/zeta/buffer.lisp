;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device buffers.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass buffer ()
  ((synchronized :initarg :synchronized
                 :reader synchronizedp)
   (device :initform nil
           :initarg :device
           :accessor device-of)
   (input-iobuf :initarg :input-buffer
                :accessor input-iobuf-of)
   (output-iobuf :initarg :output-buffer
                 :accessor output-iobuf-of)
   (buffering :initarg :buffering
              :accessor buffering-of))
  (:default-initargs :synchronized nil))

(defclass single-channel-buffer (buffer)
  ((last-io-op :initform nil :accessor last-io-op-of)))

(defclass dual-channel-buffer (buffer) ())


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

(defgeneric buffer-fill (buffer &key timeout))

(defgeneric buffer-flush (buffer &key timeout))

(defgeneric buffer-wait-until-flushable (buffer &key timeout))

(defgeneric buffer-clear-input (buffer))

(defgeneric buffer-clear-output (buffer))

;;; Internal functions

(defgeneric %buffer-read-vector (buffer vector start end timeout))

(defgeneric %buffer-write-vector (buffer vector start end timeout))

(defgeneric %buffer-fill (buffer timeout))

(defgeneric %buffer-flush (buffer timeout))

(defgeneric %buffer-clear-input (buffer))

(defgeneric %buffer-clear-output (buffer))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

(defmacro with-synchronized-buffer ((buffer &optional direction) &body body)
  (with-gensyms (body-fun)
    (labels ((make-locks (body direction)
               (ecase direction
                 (:input
                  `(bt:with-lock-held
                       ((iobuf-lock (input-iobuf-of ,buffer)))
                     ,body))
                 (:output
                  `(bt:with-lock-held
                       ((iobuf-lock (output-iobuf-of ,buffer)))
                     ,body))
                 (:io
                  (make-locks (make-locks body :output) :input)))))
      `(flet ((,body-fun () ,@body))
         (if (synchronizedp ,buffer)
             ,(make-locks `(,body-fun) direction)
             (,body-fun))))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod shared-initialize :after
    ((buffer single-channel-buffer) slot-names
     &key data size buffering)
  (declare (ignore slot-names))
  (with-accessors ((device device-of)
                   (input-iobuf input-iobuf-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (check-type device device)
    (check-type data (or null iobuf))
    (check-type buffering stream-buffering)
    (setf input-iobuf (or data (make-iobuf size))
          output-iobuf input-iobuf)))

(defmethod shared-initialize :after
    ((buffer dual-channel-buffer) slot-names
     &key input-data output-data input-size output-size buffering)
  (declare (ignore slot-names))
  (with-accessors ((device device-of)
                   (input-iobuf input-iobuf-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (check-type device device)
    (check-type input-data (or null iobuf))
    (check-type output-data (or null iobuf))
    (check-type buffering stream-buffering)
    (setf input-iobuf (or input-data (make-iobuf input-size)))
    (setf output-iobuf (or output-data (make-iobuf output-size)))))


;;;-------------------------------------------------------------------------
;;; RELINQUISH
;;;-------------------------------------------------------------------------

(defmethod relinquish ((buffer single-channel-buffer) &key abort)
  (with-accessors ((device device-of))
      buffer
    (with-synchronized-buffer (buffer :input)
      (unless (or abort (eql :read (last-io-op-of buffer)))
        (%buffer-flush buffer 0))
      (relinquish device :abort abort)))
  (values buffer))

(defmethod relinquish ((buffer dual-channel-buffer) &key abort)
  (with-accessors ((device device-of))
      buffer
    (with-synchronized-buffer (buffer :io)
      (unless abort
        (%buffer-flush buffer 0))
      (relinquish device :abort abort)))
  (values buffer))


;;;-------------------------------------------------------------------------
;;; DEVICE-READ
;;;-------------------------------------------------------------------------

(defmethod device-read :around ((buffer buffer) vector &key
                                (start 0) end timeout)
  (check-bounds vector start end)
  (if (= start end)
      0
      (call-next-method buffer vector :start start
                        :end end :timeout timeout)))

(defmethod device-read ((buffer single-channel-buffer) vector
                        &key start end timeout)
  (with-synchronized-buffer (buffer :input)
    (%buffer-read-vector buffer vector start end timeout)))

(defmethod device-read ((buffer dual-channel-buffer) vector
                        &key start end timeout)
  (with-synchronized-buffer (buffer :input)
    (%buffer-read-vector buffer vector start end timeout)))

(defmethod %buffer-read-vector ((buffer buffer) vector start end timeout)
  (with-accessors ((input-iobuf input-iobuf-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (cond
      ((iobuf-empty-p input-iobuf)
       (let ((nbytes (%buffer-fill buffer timeout)))
         (if (iobuf-empty-p input-iobuf)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->vector input-iobuf vector start end))))
      (t
       (iobuf->vector input-iobuf vector start end)))))


;;;-------------------------------------------------------------------------
;;; DEVICE-WRITE
;;;-------------------------------------------------------------------------

(defmethod device-write :around ((buffer buffer) vector
                                 &key (start 0) end timeout)
  (check-bounds vector start end)
  (if (= start end)
      0
      (call-next-method buffer vector :start start
                        :end end :timeout timeout)))

(defmethod device-write ((buffer single-channel-buffer) vector
                         &key start end timeout)
  (with-synchronized-buffer (buffer :output)
    ;; If the previous operation was a read, flush the read buffer
    ;; and reposition the file offset accordingly
    (%buffer-clear-input buffer)
    (%buffer-write-vector buffer vector start end timeout)))

(defmethod device-write ((buffer dual-channel-buffer) vector
                         &key start end timeout)
  (with-synchronized-buffer (buffer :output)
    (%buffer-write-vector buffer vector start end timeout)))

(defmethod %buffer-write-vector ((buffer buffer) vector start end timeout)
  (with-accessors ((output-iobuf output-iobuf-of))
      buffer
    (multiple-value-prog1
        (vector->iobuf output-iobuf vector start end)
      (setf (last-io-op-of buffer) :write)
      (when (iobuf-full-p output-iobuf)
        (%buffer-flush buffer timeout)))))


;;;-------------------------------------------------------------------------
;;; DEVICE-POSITION
;;;-------------------------------------------------------------------------

(defmethod device-position ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (%buffer-position buffer)))

(defun %buffer-position (buffer)
  (let ((position (device-position (device-of buffer))))
    (assert (not (null position)) (position)
            "A single-channel-buffer's device must not return a NULL device-position.")
    (ecase (last-io-op-of buffer)
      (:read
       (- position (iobuf-available-octets (input-iobuf-of buffer))))
      (:write
       (+ position (iobuf-available-octets (output-iobuf-of buffer)))))))

(defmethod (setf device-position)
    (position (buffer single-channel-buffer) &optional (from :start))
  (setf (%buffer-position buffer from) position))

(defun (setf %buffer-position) (position buffer from)
  (setf (device-position (device-of buffer) from) position))


;;;-------------------------------------------------------------------------
;;; CLEAR-INPUT
;;;-------------------------------------------------------------------------

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


;;;-------------------------------------------------------------------------
;;; CLEAR-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod buffer-clear-output ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :output)
    (%buffer-clear-output buffer)))

(defmethod %buffer-clear-output ((buffer single-channel-buffer))
  (when (eql :write (last-io-op-of buffer))
    (iobuf-reset (output-iobuf-of buffer))))

(defmethod buffer-clear-output ((buffer dual-channel-buffer))
  (with-synchronized-buffer (buffer :output)
    (iobuf-reset (output-iobuf-of buffer))))


;;;-------------------------------------------------------------------------
;;; FILL-INPUT
;;;-------------------------------------------------------------------------

(defmethod buffer-fill ((buffer single-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%buffer-clear-output buffer)
    (%buffer-fill buffer timeout)))

(defmethod buffer-fill ((buffer dual-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%buffer-fill buffer timeout)))

(defmethod %buffer-fill ((buffer buffer) timeout)
  (with-accessors ((device device-of)
                   (input-iobuf input-iobuf-of))
      buffer
    (multiple-value-bind (data start end)
        (iobuf-next-empty-zone input-iobuf)
      (let ((nbytes
             (device-read device data :start start
                          :end end :timeout timeout)))
        (etypecase nbytes
          ((eql :eof)
           (error 'end-of-file :stream buffer))
          (unsigned-byte
           (setf (iobuf-end input-iobuf) (+ start nbytes))
           (setf (last-io-op-of buffer) :read)
           (values nbytes (iobuf-available-space input-iobuf))))))))


;;;-------------------------------------------------------------------------
;;; FLUSH-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod buffer-flush ((buffer single-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :output)
    (when (eql :write (last-io-op-of buffer))
      (%buffer-flush buffer timeout))))

(defmethod buffer-flush ((buffer dual-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :output)
    (%buffer-flush buffer timeout)))

(defmethod %buffer-flush ((buffer buffer) timeout)
  (with-accessors ((device device-of)
                   (output-iobuf output-iobuf-of))
      buffer
    (multiple-value-bind (data start end)
        (iobuf-next-data-zone output-iobuf)
      (let ((nbytes
             (device-write device data :start start
                           :end end :timeout timeout)))
        (etypecase nbytes
          ((eql :hangup)
           (error 'hangup :stream buffer))
          (unsigned-byte
           (setf (iobuf-start output-iobuf) (+ start nbytes))
           (setf (last-io-op-of buffer) :write)
           (values nbytes (iobuf-available-octets output-iobuf))))))))


;;;-------------------------------------------------------------------------
;;; I/O WAIT
;;;-------------------------------------------------------------------------

(defmethod buffer-wait-until-flushable ((buffer buffer) &key timeout)
  (device-poll-output (device-of buffer) :timeout timeout))
