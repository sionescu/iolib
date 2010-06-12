;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FD mixin definitions
;;;

(in-package :iolib.streams)

(defmethod shared-initialize :after ((stream dual-channel-fd-mixin) slot-names
                                     &key fd)
  (declare (ignore slot-names))
  (check-type fd unsigned-byte)
  (setf (isys:fd-nonblock fd) t))

;;;; CLOSE

(defmethod close :around ((fd-mixin dual-channel-fd-mixin)
                          &key abort)
  (declare (ignore abort))
  (call-next-method)
  (when (fd-of fd-mixin)
    (isys:close (fd-of fd-mixin))
    (setf (fd-of fd-mixin) nil)))

;;;; Get and Set O_NONBLOCK

(defmethod fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (isys:fd-nonblock (fd-of fd-mixin)))
(defobsolete fd-non-blocking "stream FDs are now always non-blocking.")

(defmethod (setf fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (setf (isys:fd-nonblock (fd-of fd-mixin)) mode))
(defobsolete (setf fd-non-blocking) "stream FDs are now always non-blocking.")
