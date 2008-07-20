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
  `(or null non-negative-real))

(deftype stream-position () '(unsigned-byte 64))


;;;-----------------------------------------------------------------------------
;;; Generic functions
;;;-----------------------------------------------------------------------------

(defgeneric device-open (device &rest initargs))

(defgeneric device-close (device &optional abort))

(defgeneric device-read (device vector start end &optional timeout))

(defgeneric device-write (device vector start end &optional timeout))

(defgeneric device-position (device))

(defgeneric (setf device-position) (position device &rest args))

(defgeneric device-length (device))

(defgeneric wait-for-input (device &optional timeout))

(defgeneric wait-for-output (device &optional timeout))


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
;;; Get and Set O_NONBLOCK
;;;-----------------------------------------------------------------------------

(defun %get-fd-nonblock-mode (fd)
  (declare (special *device*))
  (handler-case
      (let ((current-flags (nix:fcntl fd nix:f-getfl)))
        (logtest nix:o-nonblock current-flags))
    (nix:posix-error (err)
      (posix-file-error err *device* "getting O_NONBLOCK from"))))

(defun %set-fd-nonblock-mode (fd mode)
  (declare (special *device*))
  (let* ((current-flags
          (handler-case
              (nix:fcntl fd nix:f-getfl)
            (nix:posix-error (err)
              (posix-file-error err *device* "getting O_NONBLOCK from"))))
         (new-flags (if mode
                        (logior current-flags nix:o-nonblock)
                        (logandc2 current-flags nix:o-nonblock))))
    (when (/= new-flags current-flags)
      (handler-case
          (nix:fcntl fd nix:f-setfl new-flags)
        (nix:posix-error (err)
          (posix-file-error err *device* "setting O_NONBLOCK on"))))
    (values mode)))


;;;-----------------------------------------------------------------------------
;;; I/O WAIT
;;;-----------------------------------------------------------------------------

(defmethod wait-for-input ((device device) &optional timeout)
  (iomux:wait-until-fd-ready (input-handle-of device) :input timeout))

(defmethod wait-for-output ((device device) &optional timeout)
  (iomux:wait-until-fd-ready (output-handle-of device) :output timeout))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-READ
;;;-----------------------------------------------------------------------------

(defmethod device-read :around ((device device) vector start end &optional timeout)
  (if (= start end) 0 (call-next-method)))

(defmethod device-read ((device device) vector start end &optional timeout)
  (if (and timeout (zerop timeout))
      (read-octets/non-blocking device vector start end)
      (read-octets/timeout device vector start end timeout)))

(defun read-octets/non-blocking (device vector start end)
  (declare (type device device)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (nix:repeat-upon-eintr
          (nix:read (input-handle-of device) (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (nix:posix-error (err)
        (posix-file-error err device "reading data from"))
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun read-octets/timeout (device vector start end timeout)
  (declare (type device device)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf vector)
    (nix:repeat-decreasing-timeout (remaining timeout :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (wait-for-input device remaining)
                   (return-from :rloop 0))))
        (handler-case
            (nix:read (input-handle-of device) (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (nix:posix-error (err)
            (posix-file-error err device "reading data from"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :eof nbytes))))))))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write :around ((device device) vector start end &optional timeout)
  (if (= start end) 0 (call-next-method)))

(defmethod device-write ((device device) vector start end &optional timeout)
  (if (and timeout (zerop timeout))
      (write-octets/non-blocking device vector start end)
      (write-octets/timeout device vector start end timeout)))

(defun write-octets/non-blocking (device vector start end)
  (declare (type device device)
           (type ub8-simple-vector vector)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (osicat-posix:repeat-upon-eintr
          (nix:write (output-handle-of device) (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (nix:epipe () :eof)
      (nix:posix-error (err)
        (posix-file-error err device "writing data to"))
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun write-octets/timeout (device vector start end timeout)
  (declare (type device device)
           (type ub8-simple-vector vector)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf vector)
    (nix:repeat-decreasing-timeout (remaining timeout :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (wait-for-output device remaining)
                   (return-from :rloop 0))))
        (handler-case
            (nix:write (output-handle-of device) (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (nix:epipe () (return-from :rloop :eof))
          (nix:posix-error (err)
            (posix-file-error err device "writing data to"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :eof nbytes))))))))
