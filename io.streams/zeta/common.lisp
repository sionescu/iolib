;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Common functions.
;;;

(in-package :io.zeta-streams)

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

(defmethod device-read ((device device) array start end &optional timeout)
  (when (= start end) (return-from device-read 0))
  (let ((nbytes (if (and timeout (zerop timeout))
                    (read-octets/non-blocking (input-handle-of device) array start end)
                    (read-octets/timeout (input-handle-of device) array start end timeout))))
    (cond
      ((eql :eof nbytes) (return-from device-read :eof))
      ((and (plusp nbytes) (typep device 'single-channel-device))
       (incf (device-position device) nbytes)))
    (values nbytes)))

(defun read-octets/non-blocking (input-handle array start end)
  (declare (type unsigned-byte input-handle)
           (type iobuf-data-array array)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf array)
    (handler-case
        (nix:repeat-upon-eintr
          (nix:read input-handle (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun read-octets/timeout (input-handle array start end timeout)
  (declare (type unsigned-byte input-handle)
           (type iobuf-data-array array)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf array)
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

(defmethod device-write ((device device) array start end &optional timeout)
  (when (= start end) (return-from device-write 0))
  (let ((nbytes (if (and timeout (zerop timeout))
                    (write-octets/non-blocking (output-handle-of device) array start end)
                    (write-octets/timeout (output-handle-of device) array start end timeout))))
    (cond
      ((eql :eof nbytes) (return-from device-write :eof))
      ((and (plusp nbytes) (typep device 'single-channel-device))
       (incf (device-position device) nbytes)))
    (values nbytes)))

(defun write-octets/non-blocking (output-handle array start end)
  (declare (type unsigned-byte output-handle)
           (type iobuf-data-array array)
           (type iobuf-index start end))
  (with-pointer-to-vector-data (buf array)
    (handler-case
        (osicat-posix:repeat-upon-eintr
          (nix:write output-handle (inc-pointer buf start) (- end start)))
      (nix:ewouldblock () 0)
      (nix:epipe () :eof))))

(defun write-octets/timeout (output-handle array start end timeout)
  (declare (type unsigned-byte output-handle)
           (type iobuf-data-array array)
           (type iobuf-index start end)
           (type device-timeout timeout))
  (with-pointer-to-vector-data (buf array)
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
