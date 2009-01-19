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
   (device :initarg :device
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
  ((data-vector :accessor %mb-data-vector)
   (element-type :accessor %mb-element-type)
   (input-position :initform 0
                   :accessor %mb-input-position)
   (output-position :initform 0
                   :accessor %mb-output-position)
   (adjust-size :accessor %mb-adjust-size)
   (adjust-threshold :accessor %mb-adjust-threshold)))

(defclass octet-memory-buffer (memory-buffer)
  ()
  (:default-initargs :element-type 'octet))

(defclass character-memory-buffer (memory-buffer)
  ()
  (:default-initargs :element-type 'character))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

;;; Accessors

(defgeneric zstream-synchronized-p (buffer))

(defgeneric zstream-device (buffer))

(defgeneric (setf zstream-device) (new-device buffer))

;;; I/O functions

(defgeneric zstream-read-element (buffer &key timeout))

(defgeneric zstream-write-element (buffer element &key timeout))

(defgeneric zstream-read-vector (buffer vector &key start end timeout))

(defgeneric zstream-write-vector (buffer vector &key start end timeout))

;;; Device buffer functions

(defgeneric zstream-position (buffer &key direction))

(defgeneric (setf zstream-position) (position buffer &key direction from))

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

(defgeneric %ensure-memory-buffer-capacity (buffer &optional amount))

(defgeneric %check-memory-buffer-available-data (buffer &optional amount))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod zstream-synchronized-p ((buffer device-buffer))
  (%db-synchronized-p buffer))

(defmethod zstream-synchronized-p ((buffer memory-buffer))
  (declare (ignore buffer))
  (values nil))

(defmethod zstream-device ((buffer device-buffer))
  (%db-device buffer))

(defmethod zstream-device ((buffer memory-buffer))
  (declare (ignore buffer))
  (values nil))

(defmethod (setf zstream-device) (new-device (buffer device-buffer))
  (setf (%db-device buffer) new-device))

(defmethod (setf zstream-device) (new-device (buffer memory-buffer))
  (declare (ignore new-device buffer))
  (values nil))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

;; FIXME: synchronize memory buffers too ?
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
     &key data (start 0) end (element-type t)
     (adjust-size 1.5) (adjust-threshold 1))
  (declare (ignore slot-names))
  (check-type adjust-size (real 1.001))
  (check-type adjust-threshold (real 0.1 1))
  (setf (%mb-adjust-size      buffer) adjust-size
        (%mb-adjust-threshold buffer) adjust-threshold
        (%mb-element-type     buffer) (upgraded-array-element-type
                                       element-type))
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

(defmethod relinquish :after ((buffer single-channel-buffer) &key abort)
  (with-synchronized-buffer (buffer :input)
    (unless abort
      (%zstream-flush-output buffer 0))
    (relinquish (zstream-device buffer) :abort abort))
  (values buffer))

(defmethod relinquish :after ((buffer dual-channel-buffer) &key abort)
  (with-synchronized-buffer (buffer :io)
    (unless abort
      (%zstream-flush-output buffer 0))
    (relinquish (zstream-device buffer) :abort abort))
  (values buffer))


;;;-------------------------------------------------------------------------
;;; READ-ELEMENT
;;;-------------------------------------------------------------------------

(defmethod zstream-read-element ((buffer device-buffer) &key timeout)
  (let ((v (make-array 1 :element-type 'octet)))
    (declare (dynamic-extent v))
    (zstream-read-vector buffer v :timeout timeout)
    (aref v 0)))

(defmethod zstream-read-element ((buffer memory-buffer) &key timeout)
  (declare (ignore timeout))
  (let ((v (make-array 1 :element-type (%mb-element-type buffer))))
    (declare (dynamic-extent v))
    (zstream-read-vector buffer v)
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

(defmethod %zstream-read-vector ((buffer device-buffer) vector
                                 start end timeout)
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

(defmethod zstream-read-vector ((buffer memory-buffer) vector
                                &key start end timeout)
  (declare (ignore timeout))
  (with-accessors ((data-vector %mb-data-vector)
                   (input-position %mb-input-position)
                   (output-position %mb-output-position))
      buffer
    (%check-memory-buffer-available-data buffer 1)
    (replace vector data-vector
             :start1 input-position :end1 output-position
             :start2 start :end2 end)
    (incf input-position (min (- output-position input-position)
                              (- end start)))))


;;;-------------------------------------------------------------------------
;;; WRITE-ELEMENT
;;;-------------------------------------------------------------------------

(defmethod zstream-write-element ((buffer device-buffer) octet &key timeout)
  (check-type octet octet)
  (let ((v (make-array 1 :element-type 'octet :initial-contents octet)))
    (declare (dynamic-extent v))
    (zstream-write-vector buffer v :timeout timeout)))

(defmethod zstream-write-element ((buffer memory-buffer) element &key timeout)
  (declare (ignore timeout))
  (let ((v (make-array 1 :element-type (%mb-element-type buffer)
                       :initial-contents element)))
    (declare (dynamic-extent v))
    (zstream-write-vector buffer v)))


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
  (declare (ignore vector start end timeout))
  (setf (%scb-dirtyp buffer) t))

(defmethod zstream-write-vector ((buffer memory-buffer) vector
                                 &key (start 0) end timeout)
  (declare (ignore timeout))
  (with-accessors ((data-vector %mb-data-vector)
                   (output-position %mb-output-position))
      buffer
    (%ensure-memory-buffer-capacity buffer (length vector))
    (replace data-vector vector :start1 output-position
             :start2 start :end2 end)
    (incf output-position (length vector))))


;;;-------------------------------------------------------------------------
;;; POSITION
;;;-------------------------------------------------------------------------

(defmethod zstream-position ((buffer single-channel-buffer) &key direction)
  (declare (ignore direction))
  (with-synchronized-buffer (buffer :input)
    (let ((position (device-position (zstream-device buffer))))
      ;; FIXME: signal proper condition
      (assert (not (null position)) (position)
              "A single-channel-buffer's device must not return a NULL device-position.")
      (if (%scb-dirtyp buffer)
          (+ position (iobuf-available-octets (%db-output-iobuf buffer)))
          (- position (iobuf-available-octets (%db-input-iobuf buffer)))))))

(defmethod zstream-position ((buffer dual-channel-buffer) &key direction)
  (declare (ignore direction))
  (with-synchronized-buffer (buffer :io)
    (device-position (zstream-device buffer))))

(defmethod zstream-position ((buffer memory-buffer) &key direction)
  (ecase direction
    (:input  (%mb-input-position  buffer))
    (:output (%mb-output-position buffer))))


;;;-------------------------------------------------------------------------
;;; (SETF POSITION)
;;;-------------------------------------------------------------------------

(defmethod (setf zstream-position)
    (position (buffer device-buffer) &key direction (from :start))
  (declare (ignore direction))
  (with-synchronized-buffer (buffer :input)
    (setf (%db-position buffer from) position)))

(defun (setf %db-position) (position buffer from)
  (setf (device-position (zstream-device buffer) from) position))

(defmethod (setf zstream-position)
    (offset (buffer memory-buffer) &key direction (from :start))
  (with-accessors ((data-vector %mb-data-vector)
                   (input-position %mb-input-position)
                   (output-position %mb-output-position))
      buffer
    (ecase direction
      (:input
       (let ((newpos
              (ecase from
                (:start   offset)
                (:current (+ input-position offset))
                (:output  (+ output-position offset)))))
         (check-bounds data-vector newpos output-position)
         (setf input-position newpos)))
      (:output
       (let ((newpos
              (ecase from
                (:start   offset)
                (:current (+ output-position offset))
                (:input   (+ input-position offset)))))
         (%ensure-memory-buffer-capacity buffer (- newpos output-position))
         (setf output-position newpos))))))


;;;-------------------------------------------------------------------------
;;; CLEAR-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-input ((buffer device-buffer))
  (with-synchronized-buffer (buffer :input)
    (%zstream-clear-input buffer)))

(defmethod %zstream-clear-input ((buffer single-channel-buffer))
  (unless (%scb-dirtyp buffer)
    (let ((nbytes (iobuf-available-octets (%db-input-iobuf buffer))))
      (unless (zerop nbytes)
        (setf (%db-position buffer :current) (- nbytes)))
      (iobuf-reset (%db-input-iobuf buffer)))))

(defmethod %zstream-clear-input ((buffer dual-channel-buffer))
  (iobuf-reset (%db-input-iobuf buffer)))

(defmethod zstream-clear-input ((buffer memory-buffer))
  (setf (%mb-input-position buffer) (%mb-output-position buffer)))


;;;-------------------------------------------------------------------------
;;; CLEAR-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-output ((buffer device-buffer))
  (with-synchronized-buffer (buffer :output)
    (%zstream-clear-output buffer)))

(defmethod %zstream-clear-output ((buffer single-channel-buffer))
  (when (%scb-dirtyp buffer)
    (iobuf-reset (%db-output-iobuf buffer))))

(defmethod %zstream-clear-output ((buffer dual-channel-buffer))
  (iobuf-reset (%db-output-iobuf buffer)))

(defmethod zstream-clear-output ((buffer memory-buffer))
  (setf (%mb-output-position buffer) (%mb-input-position buffer)))


;;;-------------------------------------------------------------------------
;;; FILL-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-fill-input ((buffer single-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-flush-output buffer timeout)
    (%zstream-fill-input buffer timeout)))

(defmethod zstream-fill-input ((buffer dual-channel-buffer) &key timeout)
  (with-synchronized-buffer (buffer :input)
    (%zstream-fill-input buffer timeout)))

(defmethod %zstream-fill-input ((buffer device-buffer) timeout)
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

(defmethod zstream-fill-input ((buffer memory-buffer) &key timeout)
  (declare (ignore buffer timeout))
  (values nil))


;;;-------------------------------------------------------------------------
;;; FLUSH-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-flush-output ((buffer device-buffer) &key timeout)
  (with-synchronized-buffer (buffer :output)
    (%zstream-flush-output buffer timeout)))

(defmethod %zstream-flush-output ((buffer device-buffer) timeout)
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

(defmethod zstream-flush-output ((buffer memory-buffer) &key timeout)
  (declare (ignore buffer timeout))
  (values nil))


;;;-------------------------------------------------------------------------
;;; MEMORY-BUFFER GROW
;;;-------------------------------------------------------------------------

(defmethod %ensure-memory-buffer-capacity ((buffer memory-buffer) &optional (amount 1))
  (check-type amount unsigned-byte)
  (with-accessors ((data-vector %mb-data-vector)
                   (output-position %mb-output-position)
                   (adjust-size %mb-adjust-size)
                   (adjust-threshold %mb-adjust-threshold))
      buffer
    (let* ((size-needed (+ output-position amount))
           (threshold (ceiling (* adjust-threshold size-needed))))
      (when (> threshold (length data-vector))
        (setf data-vector
              (adjust-array data-vector
                            (truncate (* adjust-size size-needed))))))))

(defmethod %check-memory-buffer-available-data ((buffer memory-buffer) &optional (amount 1))
  (check-type amount positive-integer)
  (with-accessors ((input-position %mb-input-position)
                   (output-position %mb-output-position))
      buffer
    (let ((available-data (- output-position input-position)))
      (check-type available-data unsigned-byte)
      (cond
        ((zerop available-data)
         (error 'end-of-file :stream buffer))
        ((< available-data amount)
         ;; FIXME: signal proper condition, soft EOF
         (error "~S elements requested, only ~S available"
                amount available-data))))))


;;;-------------------------------------------------------------------------
;;; I/O WAIT
;;;-------------------------------------------------------------------------

(defmethod zstream-poll ((buffer device-buffer) &key direction timeout)
  (device-poll (zstream-device buffer) direction timeout))

(defmethod zstream-poll ((buffer memory-buffer) &key direction timeout)
  (declare (ignore timeout))
  (ecase direction
    (:input (< (%mb-input-position  buffer)
               (%mb-output-position buffer)))
    (:output t)))
