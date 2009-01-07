;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Streams.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass zeta-stream ()
  ((external-format :reader external-format-of)))

(defclass single-channel-zeta-stream (single-channel-buffer zeta-stream)
  ())

(defclass dual-channel-zeta-stream (dual-channel-buffer zeta-stream)
  ())


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

(defgeneric (setf external-format-of) (external-format stream))

(defgeneric zstream-read-vector (stream sequence &key start end))

(defgeneric zstream-write-vector (stream sequence &key start end))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod (setf external-format-of)
    (external-format (stream zeta-stream))
  (setf (slot-value stream 'external-format)
        (babel:ensure-external-format external-format)))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod shared-initialize :after ((stream zeta-stream) slot-names
                                     &key (external-format :default))
  (declare (ignore slot-names))
  (setf (external-format-of stream) external-format))


;;;-------------------------------------------------------------------------
;;; READ-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-read-vector ((stream zeta-stream) (vector vector)
                                &key (start 0) end timeout)
  (check-type vector (or ub8-simple-vector ub8-vector))
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (device-read stream vector :start start :end end :timeout timeout))

(defmethod zstream-read-vector ((stream zeta-stream) (vector string)
                                &key (start 0) end timeout)
  (check-type vector (or ub8-simple-vector ub8-vector))
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  ;; TODO: write it
  )


;;;-------------------------------------------------------------------------
;;; WRITE-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-write-vector ((stream zeta-stream) (vector vector)
                                 &key (start 0) end timeout)
  (check-type vector (or ub8-simple-vector ub8-vector))
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (device-write stream vector :start start :end end :timeout timeout))

(defmethod zstream-write-vector ((stream zeta-stream) (vector string)
                                 &key (start 0) end timeout)
  (check-type vector (or ub8-simple-vector ub8-vector))
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  ;; TODO: write it
  )
