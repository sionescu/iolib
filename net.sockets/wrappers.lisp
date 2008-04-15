;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wrappers for socket-related C macros.
;;;

(in-package :net.sockets)

(c "#if defined(__linux__)")
(define "_XOPEN_SOURCE" 600)
(define "_LARGEFILE_SOURCE")
(define "_LARGEFILE64_SOURCE")
(define "_FILE_OFFSET_BITS" 64)
(c "#endif")

(include "stdlib.h")
(include "sys/socket.h")

(defwrapper* ("cmsg_space" %cmsg-space) ("unsigned" :unsigned-int)
  ((data-size ("unsigned" :unsigned-int)))
  "return CMSG_SPACE(data_size);")

(defwrapper* ("cmsg_len" %cmsg-len) ("unsigned" :unsigned-int)
  ((data-size ("unsigned" :unsigned-int)))
  "return CMSG_LEN(data_size);")

(defwrapper* ("cmsg_firsthdr" %cmsg-firsthdr) ("void*" :pointer)
  ((msg ("struct msghdr*" :pointer)))
  "return CMSG_FIRSTHDR(msg);")

(defwrapper* ("cmsg_data" %cmsg-data) ("void*" :pointer)
  ((cmsg ("struct cmsghdr*" :pointer)))
  "return CMSG_DATA(cmsg);")
