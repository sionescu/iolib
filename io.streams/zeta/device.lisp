;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Device common functions.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Device Classes and Types
;;;-----------------------------------------------------------------------------

(defclass device ()
  ((input-handle :initarg :input-handle :accessor input-handle-of)
   (output-handle :initarg :output-handle :accessor output-handle-of)))

(defclass single-channel-device (device) ())

(defclass dual-channel-device (device) ())

(defclass direct-device (single-channel-device) ())

(defclass memory-buffer-device (direct-device) ())

(defclass socket-device (dual-channel-device)
  ((domain :initarg :domain)
   (type :initarg :type)
   (protocol :initarg :protocol)))

(deftype device-timeout ()
  'non-negative-real)

(deftype stream-position () '(unsigned-byte 64))


;;;-----------------------------------------------------------------------------
;;; Generic functions
;;;-----------------------------------------------------------------------------

(defgeneric device-open (device &rest initargs))

(defgeneric device-close (device &optional abort))

(defgeneric device-read (device vector start end &optional timeout))

(defgeneric read-octets/non-blocking (device vector start end))

(defgeneric read-octets/timeout (device vector start end timeout))

(defgeneric device-write (device vector start end &optional timeout))

(defgeneric write-octets/non-blocking (device vector start end))

(defgeneric write-octets/timeout (device vector start end timeout))

(defgeneric device-position (device))

(defgeneric (setf device-position) (position device &rest args))

(defgeneric device-length (device))

(defgeneric device-poll-input (device &optional timeout))

(defgeneric device-poll-output (device &optional timeout))


;;;-----------------------------------------------------------------------------
;;; Helper macros
;;;-----------------------------------------------------------------------------

(defmacro with-device ((name) &body body)
  `(let ((*device* ,name))
     (declare (special *device*))
     ,@body))


;;;-----------------------------------------------------------------------------
;;; Default no-op methods
;;;-----------------------------------------------------------------------------

(defmethod device-position ((device device))
  (values nil))

(defmethod (setf device-position) (position (device device) &rest args)
  (declare (ignore position args))
  (values nil))

(defmethod device-length ((device device))
  (values nil))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read :around ((device device) vector start end &optional timeout)
  (declare (ignore timeout))
  (if (= start end) 0 (call-next-method)))

(defmethod device-read ((device device) vector start end &optional timeout)
  (if (and timeout (zerop timeout))
      (read-octets/non-blocking device vector start end)
      (read-octets/timeout device vector start end timeout)))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write :around ((device device) vector start end &optional timeout)
  (declare (ignore timeout))
  (if (= start end) 0 (call-next-method)))

(defmethod device-write ((device device) vector start end &optional timeout)
  (if (and timeout (zerop timeout))
      (write-octets/non-blocking device vector start end)
      (write-octets/timeout device vector start end timeout)))
