;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
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

(declaim (inline errno %set-errno))

(defwrapper* ("iolib_get_errno" errno) :int
  ()
  "return errno;")

(defwrapper* ("iolib_set_errno" %set-errno) :int
  ((value :int))
  "errno = value; return errno;")


;;;-------------------------------------------------------------------------
;;; waitpid status readers
;;;-------------------------------------------------------------------------
(include "sys/types.h" "sys/wait.h")

(declaim (inline wifexited wexitstatus wtermsig wcoredump wifstopped
                 wstopsig wifcontinued))

(defwrapper ("WIFEXITED" wifexited) :int ;; boolean
  (status :int))

(defwrapper ("WEXITSTATUS" wexitstatus) :int ;; unsigned-char
  (status :int))

(defwrapper ("WIFSIGNALED" wifsignaled) :int
  (status :int))

(defwrapper ("WTERMSIG" wtermsig) :int
  (status :int))

(defwrapper* ("iolib_wcoredump" wcoredump) :int ;; boolean
  ((status :int))
"
  #ifdef WCOREDUMP
  return WCOREDUMP(status);
  #else
  return 0;
  #endif
")

(defwrapper ("WIFSTOPPED" wifstopped) :int ;; boolean
  (status :int))

(defwrapper ("WSTOPSIG" wstopsig) :int
  (status :int))

(defwrapper ("WIFCONTINUED" wifcontinued) :int ;; boolean
  (status :int))



;;;-------------------------------------------------------------------------
;;; Socket message readers
;;;-------------------------------------------------------------------------

(include "stdlib.h") ; needed on FreeBSD to define NULL
(include "sys/socket.h")

(declaim (inline cmsg.space cmsg.len cmsg.firsthdr cmsg.data))

(defwrapper ("CMSG_SPACE" cmsg.space) :unsigned-int
  (data-size :unsigned-int))

(defwrapper ("CMSG_LEN" cmsg.len) :unsigned-int
  (data-size :unsigned-int))

(defwrapper ("CMSG_FIRSTHDR" cmsg.firsthdr) :pointer
  (msg ("struct msghdr*" :pointer)))

(defwrapper ("CMSG_DATA" cmsg.data) :pointer
  (cmsg ("struct cmsghdr*" :pointer)))


;;;-------------------------------------------------------------------------
;;; Directory listing
;;;-------------------------------------------------------------------------

(include "sys/types.h" "dirent.h")

(declaim (inline dirfd))

(defwrapper (dirfd "dirfd") :int
  (dirp ("DIR*" :pointer)))
