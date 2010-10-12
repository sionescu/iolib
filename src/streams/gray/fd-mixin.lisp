;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FD mixin definitions
;;;

(in-package :iolib.streams)

(defmethod shared-initialize :around ((stream dual-channel-fd-mixin) slot-names &key)
  (declare (ignore slot-names))
  (call-next-method)
  (setf (isys:fd-nonblock (fd-of stream)) t))

;;;; CLOSE

(defmethod close :around ((fd-mixin dual-channel-fd-mixin)
                          &key abort)
  (declare (ignore abort))
  (call-next-method)
  (when (fd-of fd-mixin)
    (isys:close (fd-of fd-mixin))
    (setf (fd-of fd-mixin) nil)))
