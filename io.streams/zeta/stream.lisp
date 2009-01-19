;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Streams.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass zstream ()
  ((external-format :reader zstream-external-format)))

(defclass device-zstream (device-buffer zstream)
  ())

(defclass single-channel-zstream (single-channel-buffer device-zstream)
  ())

(defclass dual-channel-zstream (dual-channel-buffer device-zstream)
  ())

(defclass memory-zstream (memory-buffer zstream)
  ())

(defclass octet-memory-zstream (octet-memory-buffer memory-zstream)
  ())

(defclass character-memory-zstream (character-memory-buffer memory-zstream)
  ())


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

(defgeneric (setf zstream-external-format) (external-format stream))


(defgeneric zstream-read-char (stream &key eof-error-p eof-value))

(defgeneric zstream-write-char (stream char &key hangup-error-p hangup-value))

(defgeneric zstream-read-string (stream string &key start end eof-error-p eof-value))

(defgeneric zstream-write-string (stream string &key start end hangup-error-p hangup-value))


(defgeneric zstream-read-line (stream &key eof-error-p eof-value))

(defgeneric zstream-write-line (stream line &key start end hangup-error-p hangup-value))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod (setf zstream-external-format)
    (external-format (stream zstream))
  (setf (slot-value stream 'external-format)
        (babel:ensure-external-format external-format)))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod shared-initialize :after ((stream zstream) slot-names
                                     &key (external-format :default))
  (declare (ignore slot-names))
  (setf (zstream-external-format stream) external-format))

(defun make-memory-zstream (&key data (start 0) end (element-type t)
                            (adjust-size 1.5) (adjust-threshold 1))
  (let ((element-type (upgraded-array-element-type element-type)))
    (cond
      ((subtypep element-type 'octet)
       (make-instance 'octet-memory-zstream
                      :data data :start start :end end
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold))
      ((subtypep element-type 'character)
       (make-instance 'character-memory-zstream
                      :data data :start start :end end
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold))
      ((subtypep element-type 't)
       (make-instance 'memory-zstream
                      :data data :start start :end end
                      :element-type element-type
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold))
      (t
       (error 'subtype-error :datum element-type
              :expected-supertype '(or (unsigned-byte 8) character t))))))


;;;-------------------------------------------------------------------------
;;; READ-STRING
;;;-------------------------------------------------------------------------

(defmethod zstream-read-string ((stream zstream) (string string)
                                &key (start 0) end eof-error-p eof-value)
  (check-bounds string start end)
  (when (= start end) (return* 0))
  ;; TODO: write it
  )


;;;-------------------------------------------------------------------------
;;; WRITE-OCTETS
;;;-------------------------------------------------------------------------

(defmethod zstream-write-octets ((stream zstream) (octets vector)
                                 &key (start 0) end timeout)
  (check-bounds octets start end)
  (when (= start end) (return* 0))
  (device-write stream octets :start start :end end :timeout timeout))


;;;-------------------------------------------------------------------------
;;; WRITE-STRING
;;;-------------------------------------------------------------------------

(defmethod zstream-write-string ((stream zstream) (string string)
                                 &key (start 0) end hangup-error-p hangup-value)
  (check-bounds string start end)
  (when (= start end) (return* 0))
  ;; TODO: write it
  )
