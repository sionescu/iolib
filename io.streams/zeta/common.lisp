;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Common functions.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Default no-op methods
;;;-----------------------------------------------------------------------------

(defmethod device-clear-input ((device device))
  (values device))

(defmethod device-clear-output ((device device))
  (values device))

(defmethod device-flush-output ((device device) &optional timeout)
  (declare (ignore timeout))
  (values device))

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

(defmethod device-read ((device device) buffer start end &optional (timeout nil timeoutp))
  (when (= start end) (return-from device-read 0))
  (let* ((timeout (if timeoutp timeout (input-timeout-of device)))
         (nbytes (if (and timeout (zerop timeout))
                     (read-octets-non-blocking (input-handle-of device) buffer start end)
                     (read-octets-with-timeout (input-handle-of device) buffer start end timeout))))
    (when (plusp nbytes) (incf (device-position device) nbytes))
    (values nbytes)))

(defun read-octets-non-blocking (fd buffer start end)
  (declare (type unsigned-byte fd)
           (type ub8-sarray buffer)
           (type unsigned-byte start end))
  (with-pointer-to-vector-data (buf buffer)
    (handler-case
        (nix:repeat-upon-eintr
          (nix:read fd (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun read-octets-with-timeout (fd buffer start end timeout)
  (declare (type unsigned-byte fd)
           (type ub8-sarray buffer)
           (type unsigned-byte start end))
  (with-pointer-to-vector-data (buf buffer)
    (nix:repeat-decreasing-timeout (remaining timeout nil)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (iomux:wait-until-fd-ready fd :input remaining)
                   (return 0))))
        (handler-case
            (nix:read fd (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (:no-error (nbytes)
            (if (zerop nbytes) :eof nbytes)))))))


;;;-----------------------------------------------------------------------------
;;; Default DEVICE-WRITE
;;;-----------------------------------------------------------------------------

(defmethod device-write ((device device) buffer start end &optional (timeout nil timeoutp))
  (when (= start end) (return-from device-write 0))
  (let* ((timeout (if timeoutp timeout (output-timeout-of device)))
         (nbytes (if (and timeout (zerop timeout))
                     (write-octets-non-blocking (output-handle-of device) buffer start end)
                     (write-octets-with-timeout (output-handle-of device) buffer start end timeout))))
    (when (plusp nbytes) (incf (device-position device) nbytes))
    (values nbytes)))

(defun write-octets-non-blocking (fd buffer start end)
  (declare (type unsigned-byte fd)
           (type ub8-sarray buffer)
           (type unsigned-byte start end))
  (with-pointer-to-vector-data (buf buffer)
    (handler-case
        (osicat-posix:repeat-upon-eintr
          (nix:write fd (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun write-octets-with-timeout (fd buffer start end timeout)
  (declare (type unsigned-byte fd)
           (type ub8-sarray buffer)
           (type unsigned-byte start end))
  (with-pointer-to-vector-data (buf buffer)
    (nix:repeat-decreasing-timeout (remaining timeout nil)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (iomux:wait-until-fd-ready fd :output remaining)
                   (return 0))))
        (handler-case
            (nix:write fd (inc-pointer buf start) (- end start))
          (nix:eintr () (check-timeout))
          (nix:ewouldblock () (check-timeout))
          (:no-error (nbytes)
            (return (if (zerop nbytes) :eof nbytes))))))))
