;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FD mixin definitions
;;;

(in-package :iolib.streams)

;;;; CLOSE

(defmethod close :around ((fd-mixin dual-channel-single-fd-mixin)
                          &key abort)
  (declare (ignore abort))
  (call-next-method)
  (when (fd-of fd-mixin)
    (isys:close (fd-of fd-mixin))
    (setf (fd-of fd-mixin) nil)))

;;;; Get and Set O_NONBLOCK

(defmethod input-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (isys:fd-nonblock (fd-of fd-mixin)))
(defobsolete input-fd-non-blocking "stream FDs are now always non-blocking.")

(defmethod (setf input-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (setf (isys:fd-nonblock (fd-of fd-mixin)) mode))
(defobsolete (setf input-fd-non-blocking) "stream FDs are now always non-blocking.")

(defmethod output-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (isys:fd-nonblock (output-fd-of fd-mixin)))
(defobsolete output-fd-non-blocking "stream FDs are now always non-blocking.")

(defmethod (setf output-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (setf (isys:fd-nonblock (output-fd-of fd-mixin)) mode))
(defobsolete (setf output-fd-non-blocking) "stream FDs are now always non-blocking.")

(defmethod fd-non-blocking ((fd-mixin dual-channel-single-fd-mixin))
  (isys:fd-nonblock (fd-of fd-mixin)))
(defobsolete fd-non-blocking "stream FDs are now always non-blocking.")

(defmethod (setf fd-non-blocking) (mode (fd-mixin dual-channel-single-fd-mixin))
  (check-type mode boolean "a boolean value")
  (setf (isys:fd-nonblock (fd-of fd-mixin)) mode))
(defobsolete (setf fd-non-blocking) "stream FDs are now always non-blocking.")
