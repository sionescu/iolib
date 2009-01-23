;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FFI wrappers.
;;;

(in-package :iolib.syscalls)

(c "#if defined(__linux__)")
(c "#undef _GNU_SOURCE")
(define "_XOPEN_SOURCE" 600)
(define "_LARGEFILE_SOURCE")
(define "_LARGEFILE64_SOURCE")
(define "_FILE_OFFSET_BITS" 64)
(c "#endif")


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(include "errno.h")

(declaim (inline %sys-errno))
(defwrapper* ("iolib_get_errno" %sys-errno) :int
  ()
  "return errno;")

(declaim (inline %%sys-set-errno))
(defwrapper* ("iolib_set_errno" %%sys-set-errno) :int
  ((value :int))
  "errno = value;"
  "return errno;")


;;;-------------------------------------------------------------------------
;;; Socket message readers
;;;-------------------------------------------------------------------------

(include "stdlib.h") ; needed on FreeBSD to define NULL
(include "sys/socket.h")

(defwrapper* ("cmsg_space" %sys-cmsg-space) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_SPACE(data_size);")

(defwrapper* ("cmsg_len" %sys-cmsg-len) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_LEN(data_size);")

(defwrapper* ("cmsg_firsthdr" %sys-cmsg-firsthdr) :pointer
  ((msg ("struct msghdr*" :pointer)))
  "return CMSG_FIRSTHDR(msg);")

(defwrapper* ("cmsg_data" %sys-cmsg-data) :pointer
  ((cmsg ("struct cmsghdr*" :pointer)))
  "return CMSG_DATA(cmsg);")
