;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FD mixin definitions
;;;

(in-package :io.streams)

;;;; CLOSE

(defmethod close :around ((fd-mixin dual-channel-single-fd-mixin)
                          &key abort)
  (declare (ignore abort))
  (call-next-method)
  (when (fd-of fd-mixin)
    (isys:%sys-close (fd-of fd-mixin))
    (setf (fd-of fd-mixin) nil)))

;;;; Get and Set O_NONBLOCK

(defun %get-fd-nonblock-mode (fd)
  (let ((current-flags (isys:%sys-fcntl fd nix:f-getfl)))
    (logtest isys:o-nonblock current-flags)))

(defun %set-fd-nonblock-mode (fd mode)
  (let* ((current-flags (isys:%sys-fcntl fd nix:f-getfl))
         (new-flags (if mode
                        (logior current-flags isys:o-nonblock)
                        (logandc2 current-flags isys:o-nonblock))))
    (when (/= new-flags current-flags)
      (isys:%sys-fcntl fd isys:f-setfl new-flags))
    (values mode)))

(defmethod input-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (%get-fd-nonblock-mode (fd-of fd-mixin)))

(defmethod (setf input-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (fd-of fd-mixin) mode))

(defmethod output-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (%get-fd-nonblock-mode (output-fd-of fd-mixin)))

(defmethod (setf output-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (output-fd-of fd-mixin) mode))

(defmethod fd-non-blocking ((fd-mixin dual-channel-single-fd-mixin))
  (%get-fd-nonblock-mode (fd-of fd-mixin)))

(defmethod (setf fd-non-blocking) (mode (fd-mixin dual-channel-single-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (fd-of fd-mixin) mode))
