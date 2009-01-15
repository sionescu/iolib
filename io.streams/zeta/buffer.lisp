;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device buffers.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass buffer ()
  ())

(defclass device-buffer (buffer)
  ((synchronized :initarg :synchronized
                 :reader %db-synchronized-p)
   (device :initform nil
           :initarg :device
           :accessor %db-device)
   (input-iobuf :initarg :input-buffer
                :accessor %db-input-iobuf)
   (output-iobuf :initarg :output-buffer
                 :accessor %db-output-iobuf)
   (buffering :initarg :buffering
              :accessor %db-buffering))
  (:default-initargs :synchronized nil))

(defclass single-channel-buffer (device-buffer)
  ((dirtyp :initform nil
           :accessor %scb-dirtyp)))

(defclass dual-channel-buffer (device-buffer)
  ())

(defclass memory-buffer (buffer)
  ((data-vector :initarg :data
                :accessor %mb-data-vector)
   (input-position :initform 0
                   :accessor %mb-input-position)
   (output-position :initform 0
                   :accessor %mb-output-position)
   (adjust-size :initarg :adjust-size
                :accessor %mb-adjust-size)
   (adjust-threshold :initarg :adjust-threshold
                     :accessor %mb-adjust-threshold)))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

;;; Accessors

(defgeneric zstream-synchronized-p (buffer))

(defgeneric zstream-device (buffer))

(defgeneric (setf zstream-device) (new-device buffer))

;;; I/O functions

(defgeneric zstream-read-octet (buffer &key timeout))

(defgeneric zstream-write-octet (buffer byte &key timeout))

(defgeneric zstream-read-vector (buffer vector &key start end timeout))

(defgeneric zstream-write-vector (buffer vector &key start end timeout))

;;; Device buffer functions

(defgeneric zstream-io-position (buffer))

(defgeneric (setf zstream-io-position) (position buffer &optional from))

(defgeneric zstream-input-position (buffer))

(defgeneric (setf zstream-input-position) (position buffer &optional from))

(defgeneric zstream-output-position (buffer))

(defgeneric (setf zstream-output-position) (position buffer &optional from))

(defgeneric zstream-poll (buffer &key direction timeout))

(defgeneric zstream-fill-input (buffer &key timeout))

(defgeneric zstream-flush-output (buffer &key timeout))

(defgeneric zstream-clear-input (buffer))

(defgeneric zstream-clear-output (buffer))

;;; Internal functions

(defgeneric %zstream-read-vector (buffer vector start end timeout))

(defgeneric %zstream-write-vector (buffer vector start end timeout))

(defgeneric %zstream-fill-input (buffer timeout))

(defgeneric %zstream-flush-output (buffer timeout))

(defgeneric %zstream-clear-input (buffer))

(defgeneric %zstream-clear-output (buffer))

(defgeneric %ensure-memory-buffer-capacity (buffer amount))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod zstream-synchronized-p ((buffer device-buffer))
  (%db-synchronized-p buffer))

(defmethod zstream-device ((buffer device-buffer))
  (%db-device buffer))

(defmethod (setf zstream-device) (new-device (buffer device-buffer))
  (setf (%db-device buffer) new-device))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

(defmacro with-synchronized-buffer ((buffer &optional direction) &body body)
  (with-gensyms (body-fun)
    (labels ((make-locks (body direction)
               (ecase direction
                 (:input
                  `(bt:with-lock-held
                       ((iobuf-lock (%db-input-iobuf ,buffer)))
                     ,body))
                 (:output
                  `(bt:with-lock-held
                       ((iobuf-lock (%db-output-iobuf ,buffer)))
                     ,body))
                 (:io
                  (make-locks (make-locks body :output) :input)))))
      `(flet ((,body-fun () ,@body))
         (if (zstream-synchronized-p ,buffer)
             ,(make-locks `(,body-fun) direction)
             (,body-fun))))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod shared-initialize :after
    ((buffer single-channel-buffer) slot-names
     &key data size buffering)
  (declare (ignore slot-names))
  (with-accessors ((device zstream-device)
                   (input-iobuf %db-input-iobuf)
                   (output-iobuf %db-output-iobuf))
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
  (with-accessors ((device zstream-device)
                   (input-iobuf %db-input-iobuf)
                   (output-iobuf %db-output-iobuf))
      buffer
    (check-type device device)
    (check-type input-data (or null iobuf))
    (check-type output-data (or null iobuf))
    (check-type buffering stream-buffering)
    (setf input-iobuf (or input-data (make-iobuf input-size)))
    (setf output-iobuf (or output-data (make-iobuf output-size)))))

(defmethod shared-initialize :after
    ((buffer memory-buffer) slot-names
     &key data (start 0) end element-type
     (adjust-size 1.5) (adjust-threshold 1))
  (declare (ignore slot-names))
  ;; FIXME: signal proper condition
  (assert (> adjust-size 1))
  (assert (<= adjust-threshold 1))
  (cond
    (data
     (check-bounds data start end)
     (when element-type
       ;; FIXME: signal proper condition
       (assert (subtypep element-type (array-element-type data))))
     (setf (%mb-data-vector buffer)
           (make-array (truncate (* adjust-size (length data)))
                       :element-type (or element-type
                                         (array-element-type data))))
     (setf (%mb-output-position buffer) (- end start))
     (replace (%mb-data-vector buffer) data :start2 start :end2 end))
    (t
     (setf (%mb-data-vector buffer)
           (make-array 128 :element-type element-type)))))


;;;-------------------------------------------------------------------------
;;; RELINQUISH
;;;-------------------------------------------------------------------------

(defmethod relinquish ((buffer single-channel-buffer) &key abort)
  (with-synchronized-buffer (buffer :input)
    (unless abort
      (%zstream-flush-output buffer 0))
    (relinquish (zstream-device buffer) :abort abort))
  (values buffer))

(defmethod relinquish ((buffer dual-channel-buffer) &key abort)
  (with-synchronized-buffer (buffer :io)
    (unless abort
      (%zstream-flush-output buffer 0))
    (relinquish (zstream-device buffer) :abort abort))
  (values buffer))


;;;-------------------------------------------------------------------------
;;; READ-OCTET
;;;-------------------------------------------------------------------------

(defmethod zstream-read-octet ((buffer buffer) &key timeout)
  (let ((v (make-array 1 :element-type 'ub8)))
    (declare (dynamic-extent v))
    (zstream-read-vector buffer v :timeout timeout)
    (aref v 0)))


;;;-------------------------------------------------------------------------
;;; READ-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-read-vector :around ((buffer buffer) vector &key
                                        (start 0) end timeout)
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (call-next-method buffer vector :start start :end end :timeout timeout))

(defmethod zstream-read-vector ((buffer single-channel-buffer) vector
                                &key start end timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-read-vector buffer vector start end timeout)))

(defmethod zstream-read-vector ((buffer dual-channel-buffer) vector
                                &key start end timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-read-vector buffer vector start end timeout)))

(defmethod %zstream-read-vector ((buffer device-buffer) vector start end timeout)
  (with-accessors ((input-iobuf %db-input-iobuf))
      buffer
    (cond
      ((iobuf-empty-p input-iobuf)
       (let ((nbytes (%zstream-fill-input buffer timeout)))
         (if (iobuf-empty-p input-iobuf)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->vector input-iobuf vector start end))))
      (t
       (iobuf->vector input-iobuf vector start end)))))


;;;-------------------------------------------------------------------------
;;; WRITE-OCTET
;;;-------------------------------------------------------------------------

(defmethod zstream-write-octet ((buffer buffer) octet &key timeout)
  (check-type octet (unsigned-byte 8))
  (let ((v (make-array 1 :element-type 'ub8 :initial-contents octet)))
    (declare (dynamic-extent v))
    (zstream-write-vector buffer v :timeout timeout)))


;;;-------------------------------------------------------------------------
;;; WRITE-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-write-vector :around ((buffer buffer) vector
                                         &key (start 0) end timeout)
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (call-next-method buffer vector :start start :end end :timeout timeout))

(defmethod zstream-write-vector ((buffer single-channel-buffer) vector
                                 &key start end timeout)
  (with-synchronized-buffer (buffer :output)
    ;; If the previous operation was a read, flush the read buffer
    ;; and reposition the file offset accordingly
    (%zstream-clear-input buffer)
    (%zstream-write-vector buffer vector start end timeout)))

(defmethod zstream-write-vector ((buffer dual-channel-buffer) vector
                                 &key start end timeout)
  (with-synchronized-buffer (buffer :output)
    (%zstream-write-vector buffer vector start end timeout)))

(defmethod %zstream-write-vector ((buffer device-buffer) vector start end timeout)
  (with-accessors ((output-iobuf %db-output-iobuf))
      buffer
    (multiple-value-prog1
        (vector->iobuf output-iobuf vector start end)
      (when (iobuf-full-p output-iobuf)
        (%zstream-flush-output buffer timeout)))))

(defmethod %zstream-write-vector :after ((buffer single-channel-buffer)
                                         vector start end timeout)
  (setf (%scb-dirtyp buffer) t))

(defmethod zstream-write-vector ((buffer memory-buffer) vector
                                 &key (start 0) end timeout)
  (declare (ignore timeout))
  (check-bounds vector start end)
  (with-accessors ((data-vector %mb-data-vector)
                   (output-position %mb-output-position))
      buffer
    (%ensure-memory-buffer-capacity buffer (length vector))
    (replace data-vector vector :start1 output-position
             :start2 start :end2 end)
    (incf output-position (length vector))))


;;;-------------------------------------------------------------------------
;;; IO-POSITION
;;;-------------------------------------------------------------------------

(defmethod zstream-io-position ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (let ((position (device-position (zstream-device buffer))))
      ;; FIXME: signal proper condition
      (assert (not (null position)) (position)
              "A single-channel-buffer's device must not return a NULL device-position.")
      (if (%scb-dirtyp buffer)
          (+ position (iobuf-available-octets (%db-output-iobuf buffer)))
          (- position (iobuf-available-octets (%db-input-iobuf buffer)))))))

(defmethod zstream-io-position ((buffer dual-channel-buffer))
  (device-position (zstream-device buffer)))

(defmethod zstream-io-position ((buffer memory-buffer))
  (declare (ignore buffer))
  ;; FIXME: signal an error because it has two cursors ?
  nil)

(defmethod (setf zstream-io-position)
    (position (buffer single-channel-buffer) &optional (from :start))
  (setf (device-position (zstream-device buffer) from) position))

(defmethod (setf zstream-io-position)
    (position (buffer dual-channel-buffer) &optional (from :start))
  (setf (device-position (zstream-device buffer) from) position))

(defmethod (setf zstream-io-position)
    (position (buffer dual-channel-buffer) &optional (from :start))
  (declare (ignore position buffer from))
  ;; FIXME: signal an error because it has two cursors ?
  nil)


;;;-------------------------------------------------------------------------
;;; INPUT-POSITION
;;;-------------------------------------------------------------------------

(defmethod zstream-input-position ((buffer memory-buffer))
  (%mb-input-position buffer))

(defmethod (setf zstream-input-position)
    (offset (buffer memory-buffer) &optional (from :start))
  (with-accessors ((data-vector %mb-data-vector)
                   (input-position %mb-input-position)
                   (output-position %mb-output-position))
      buffer
    (let ((len (length data-vector))
          (newpos
           (ecase from
             (:start   offset)
             (:current (+ input-position offset))
             (:output  (+ output-position offset)))))
      ;; FIXME: signal proper condition
      (assert (< output-position len))
      (unless (and (<= newpos output-position))
        ;; FIXME: signal proper condition
        (error "Wrong sequence bounds. start: ~S end: ~S"
               newpos output-position))
      (setf input-position newpos))))


;;;-------------------------------------------------------------------------
;;; OUTPUT-POSITION
;;;-------------------------------------------------------------------------

(defmethod zstream-output-position ((buffer memory-buffer))
  (%mb-output-position buffer))

(defmethod (setf zstream-output-position)
    (offset (buffer memory-buffer) &optional (from :start))
  (with-accessors ((data-vector %mb-data-vector)
                   (input-position %mb-input-position)
                   (output-position %mb-output-position)
                   (adjust-size %mb-adjust-size))
      buffer
    (let ((newpos
           (ecase from
             (:start   offset)
             (:current (+ output-position offset))
             (:input   (+ input-position offset)))))
      (unless (<= input-position newpos)
        ;; FIXME: signal proper condition
        (error "Wrong sequence bounds. start: ~S end: ~S"
               input-position newpos))
      (%ensure-memory-buffer-capacity buffer (- newpos output-position))
      (setf output-position newpos))))


;;;-------------------------------------------------------------------------
;;; CLEAR-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-input ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :input)
    (%zstream-clear-input buffer)))

(defmethod %zstream-clear-input ((buffer single-channel-buffer))
  (unless (%scb-dirtyp buffer)
    (let ((nbytes (iobuf-available-octets (%db-input-iobuf buffer))))
      (unless (zerop nbytes)
        (setf (%buffer-position buffer :current) (- nbytes)))
      (iobuf-reset (%db-input-iobuf buffer)))))

(defmethod zstream-clear-input ((buffer buffer))
  (with-synchronized-buffer (buffer :input)
    (%zstream-clear-input buffer)))

(defmethod %zstream-clear-input ((buffer dual-channel-buffer))
  (iobuf-reset (%db-input-iobuf buffer)))


;;;-------------------------------------------------------------------------
;;; CLEAR-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-output ((buffer single-channel-buffer))
  (with-synchronized-buffer (buffer :output)
    (%zstream-clear-output buffer)))

(defmethod %zstream-clear-output ((buffer single-channel-buffer))
  (when (%scb-dirtyp buffer)
    (iobuf-reset (%db-output-iobuf buffer))))

(defmethod zstream-clear-output ((buffer dual-channel-buffer))
  (with-synchronized-buffer (buffer :output)
    (iobuf-reset (%db-output-iobuf buffer))))


;;;-------------------------------------------------------------------------
;;; FILL-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-fill-input ((buffer single-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-flush-output buffer)
    (%zstream-fill-input buffer timeout)))

(defmethod zstream-fill-input ((buffer dual-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-fill-input buffer timeout)))

(defmethod %zstream-fill-input ((buffer buffer) timeout)
  (with-accessors ((device zstream-device)
                   (input-iobuf %db-input-iobuf))
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
           (values nbytes (iobuf-available-space input-iobuf))))))))


;;;-------------------------------------------------------------------------
;;; FLUSH-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-flush-output ((buffer device-buffer) &key timeout)
  (with-synchronized-buffer (buffer :output)
    (%zstream-flush-output buffer timeout)))

(defmethod %zstream-flush-output ((buffer buffer) timeout)
  (with-accessors ((device zstream-device)
                   (output-iobuf %db-output-iobuf))
      buffer
    (when (%scb-dirtyp buffer)
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
             (values nbytes (iobuf-available-octets output-iobuf)))))))))

(defmethod %zstream-flush-output :after ((buffer single-channel-buffer) timeout)
  (declare (ignore timeout))
  (when (iobuf-empty-p (%db-output-iobuf buffer))
    (setf (%scb-dirtyp buffer) nil)))


;;;-------------------------------------------------------------------------
;;; MEMORY-BUFFER GROW
;;;-------------------------------------------------------------------------

(defmethod %ensure-memory-buffer-capacity ((buffer memory-buffer) amount)
  (check-type amount unsigned-byte)
  (with-accessors ((data-vector %mb-data-vector)
                   (output-position %mb-output-position)
                   (adjust-size %mb-adjust-size)
                   (adjust-threshold %mb-adjust-threshold))
      buffer
    (let* ((size-needed (+ output-position amount))
           (threshold (* adjust-threshold size-needed)))
      (when (>= threshold (length data-vector))
        (setf data-vector
              (adjust-array data-vector (truncate (* adjust-size
                                                     size-needed))))))))


;;;-------------------------------------------------------------------------
;;; I/O WAIT
;;;-------------------------------------------------------------------------

(defmethod zstream-poll ((buffer buffer) &key direction timeout)
  (device-poll (zstream-device buffer) direction timeout))
