;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wrappers for socket-related C macros.
;;;

(in-package :iolib.sockets)

(c "#if defined(__linux__)")
(define "_XOPEN_SOURCE" 600)
(define "_LARGEFILE_SOURCE")
(define "_LARGEFILE64_SOURCE")
(define "_FILE_OFFSET_BITS" 64)
(c "#endif")

(include "stdlib.h") ; needed on FreeBSD to define NULL
(include "sys/socket.h")

(defwrapper* ("cmsg_space" %cmsg-space) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_SPACE(data_size);")

(defwrapper* ("cmsg_len" %cmsg-len) :unsigned-int
  ((data-size :unsigned-int))
  "return CMSG_LEN(data_size);")

(defwrapper* ("cmsg_firsthdr" %cmsg-firsthdr) :pointer
  ((msg ("struct msghdr*" :pointer)))
  "return CMSG_FIRSTHDR(msg);")

(defwrapper* ("cmsg_data" %cmsg-data) :pointer
  ((cmsg ("struct cmsghdr*" :pointer)))
  "return CMSG_DATA(cmsg);")
