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
;;; Memory manipulation
;;;-------------------------------------------------------------------------

(defcfun (memset "memset") :pointer
  "Fill the first COUNT bytes of BUFFER with the constant VALUE."
  (buffer :pointer)
  (value  :int)
  (count  size-t))

(defun bzero (buffer count)
  "Fill the first COUNT bytes of BUFFER with zeros."
  (memset buffer 0 count))

(defcfun (memcpy "memcpy") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas must not overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))

(defcfun (memmove "memmove") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas may overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))

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
