;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Zeta streams classes.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Types
;;;-----------------------------------------------------------------------------

(deftype ub8  () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype sb8  () '(signed-byte 8))
(deftype sb16 () '(signed-byte 16))
(deftype sb32 () '(signed-byte 32))

(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))

(deftype ub8-vector () '(vector ub8))

(deftype ub16-sarray (&optional (size '*))
  `(simple-array ub16 (,size)))


;;;-----------------------------------------------------------------------------
;;; Device Classes
;;;-----------------------------------------------------------------------------

(deftype stream-position () '(unsigned-byte 64))

(defclass device ()
  ((input-handle :initarg :input-handle :accessor input-handle-of)
   (input-timeout :initarg :input-timeout :accessor input-timeout-of)
   (output-handle :initarg :output-handle :accessor output-handle-of)
   (output-timeout :initarg :output-timeout :accessor output-timeout-of))
  (:default-initargs :input-timeout nil
                     :output-timeout nil))

(defclass single-channel-device (device)
  ((position :initarg :position :accessor position-of))
  (:default-initargs :position 0))

(defclass dual-channel-device (device) ())

(defclass file-device (single-channel-device)
  ((filename :initarg :filename :accessor filename-of)
   (direction :initarg :direction :accessor direction-of)
   (if-exists :initarg :if-exists :accessor if-exists-of)
   (if-does-not-exist :initarg :if-does-not-exist :accessor if-does-not-exist-of)))

(defclass direct-device (single-channel-device) ())

(defclass memory-mapped-file-device (file-device direct-device) ())

(defclass memory-buffer-device (direct-device) ())

(defclass socket-device (dual-channel-device)
  ((domain :initarg :domain)
   (type :initarg :type)
   (protocol :initarg :protocol)))


;;;-----------------------------------------------------------------------------
;;; Generic functions
;;;-----------------------------------------------------------------------------

(defgeneric device-open (device &rest initargs))

(defgeneric device-close (device))

(defgeneric device-read (device buffer start end &optional timeout))

(defgeneric device-write (device buffer start end &optional timeout))

(defgeneric device-clear-input (device))

(defgeneric device-clear-output (device))

(defgeneric device-flush-output (device &optional timeout))

(defgeneric device-position (device))

(defgeneric (setf device-position) (position device &rest args))

(defgeneric device-length (device))
