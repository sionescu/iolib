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
   (output-handle :initarg :output-handle :accessor output-handle-of))
  (:default-initargs :input-timeout nil
                     :output-timeout nil))

(defclass single-channel-device (device) ())

(defclass dual-channel-device (device) ())

(defclass direct-device (single-channel-device) ())

(defclass memory-buffer-device (direct-device) ())

(defclass socket-device (dual-channel-device)
  ((domain :initarg :domain)
   (type :initarg :type)
   (protocol :initarg :protocol)))

(deftype device-timeout ()
  `(or null non-negative-real))

(deftype stream-position () '(unsigned-byte 64))


;;;-----------------------------------------------------------------------------
;;; Generic functions
;;;-----------------------------------------------------------------------------

(defgeneric device-open (device &rest initargs))

(defgeneric device-close (device))

(defgeneric device-read (device vector start end &optional timeout))

(defgeneric device-write (device vector start end &optional timeout))

(defgeneric device-position (device))

(defgeneric (setf device-position) (position device &rest args))

(defgeneric device-length (device))


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
;;; Get and Set O_NONBLOCK
;;;-----------------------------------------------------------------------------

(defun %get-fd-nonblock-mode (fd)
  (let ((current-flags (nix:fcntl fd nix:f-getfl)))
    (logtest nix:o-nonblock current-flags)))

(defun %set-fd-nonblock-mode (fd mode)
  (let* ((current-flags (nix:fcntl fd nix:f-getfl))
         (new-flags (if mode
                        (logior current-flags nix:o-nonblock)
                        (logandc2 current-flags nix:o-nonblock))))
    (when (/= new-flags current-flags)
      (nix:fcntl fd nix:f-setfl new-flags))
    (values mode)))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read ((device device) vector start end &optional timeout)
  (when (= start end) (return-from device-read 0))
  (if (and timeout (zerop timeout))
      (read-octets/non-blocking (input-handle-of device) vector start end)
      (read-octets/timeout (input-handle-of device) vector start end timeout)))

(defun read-octets/non-blocking (input-handle vector start end)
  (declare (type unsigned-byte input-handle)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (nix:repeat-upon-eintr
          (nix:read input-handle (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun read-octets/timeout (input-handle vector start end timeout)
  (declare (type unsigned-byte input-handle)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf vector)
    (nix:repeat-decreasing-timeout (remaining timeout :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (iomux:wait-until-fd-ready input-handle :input remaining)
                   (return-from :rloop 0))))
        (handler-case
            (nix:read input-handle (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (:no-error (nbytes)
            (if (zerop nbytes) :eof nbytes)))))))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write ((device device) vector start end &optional timeout)
  (when (= start end) (return-from device-write 0))
  (if (and timeout (zerop timeout))
      (write-octets/non-blocking (output-handle-of device) vector start end)
      (write-octets/timeout (output-handle-of device) vector start end timeout)))

(defun write-octets/non-blocking (output-handle vector start end)
  (declare (type unsigned-byte output-handle)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (osicat-posix:repeat-upon-eintr
          (nix:write output-handle (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (nix:epipe () :eof))))

(defun write-octets/timeout (output-handle vector start end timeout)
  (declare (type unsigned-byte output-handle)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf vector)
    (nix:repeat-decreasing-timeout (remaining timeout :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (iomux:wait-until-fd-ready output-handle :output remaining)
                   (return-from :rloop 0))))
        (handler-case
            (nix:write output-handle (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (nix:epipe () :eof))))))
