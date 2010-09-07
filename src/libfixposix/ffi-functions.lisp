;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign function definitions
;;;

(in-package :libfixposix)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libfixposix
    (:unix "libfixposix.so"))
  (use-foreign-library libfixposix))


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(defcfun (errno "lfp_errno") :int)

(defun (setf errno) (value)
  (foreign-funcall "lfp_set_errno" :int value :int))


;;;-------------------------------------------------------------------------
;;; Socket message readers
;;;-------------------------------------------------------------------------

(defcfun (cmsg.firsthdr "lfp_cmsg_firsthdr") :pointer
  (msgh :pointer))

(defcfun (cmsg.nxthdr "lfp_cmsg_nxthdr") :pointer
  (msgh :pointer)
  (cmsg :pointer))

(defcfun (cmsg.align "lfp_cmsg_align") size-t
  (length size-t))

(defcfun (cmsg.space "lfp_cmsg_space") size-t
  (length size-t))

(defcfun (cmsg.len "lfp_cmsg_len") size-t
  (length size-t))

(defcfun (cmsg.data "lfp_cmsg_data") :pointer
  (cmsg :pointer))
