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

(declaim (inline errno))
(defwrapper* ("iolib_get_errno" errno) :int
  ()
  "return errno;")

(declaim (inline %set-errno))
(defwrapper* ("iolib_set_errno" %set-errno) :int
  ((value :int))
  "errno = value;"
  "return errno;")


;;;-------------------------------------------------------------------------
;;; Socket message readers
;;;-------------------------------------------------------------------------

(include "stdlib.h") ; needed on FreeBSD to define NULL
(include "sys/socket.h")

(declaim (inline cmsg.space))
(defwrapper* ("cmsg_space" cmsg.space) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_SPACE(data_size);")

(declaim (inline cmsg.len))
(defwrapper* ("cmsg_len" cmsg.len) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_LEN(data_size);")

(declaim (inline cmsg.firsthdr))
(defwrapper* ("cmsg_firsthdr" cmsg.firsthdr) :pointer
  ((msg ("struct msghdr*" :pointer)))
  "return CMSG_FIRSTHDR(msg);")

(declaim (inline cmsg.data))
(defwrapper* ("cmsg_data" cmsg.data) :pointer
  ((cmsg ("struct cmsghdr*" :pointer)))
  "return CMSG_DATA(cmsg);")


;;;-------------------------------------------------------------------------
;;; Directory listing
;;;-------------------------------------------------------------------------

(include "sys/types.h" "dirent.h")

(declaim (inline dirfd))
(defwrapper (dirfd "dirfd") :int
  (dirp ("DIR*" :pointer)))
