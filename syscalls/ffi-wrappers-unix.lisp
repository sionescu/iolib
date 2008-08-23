;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FFI wrappers.
;;;

(in-package :iolib.syscalls)

(c "#if defined(__linux__)")
(define "_XOPEN_SOURCE" 600)
(define "_LARGEFILE_SOURCE")
(define "_LARGEFILE64_SOURCE")
(define "_FILE_OFFSET_BITS" 64)
(c "#endif")

(include "string.h" "errno.h" "sys/types.h" "sys/stat.h"
         "unistd.h" "sys/mman.h")


;;;-----------------------------------------------------------------------------
;;; Large-file support
;;;-----------------------------------------------------------------------------

;;; FIXME: this is only necessary on Linux right?

(defwrapper ("lseek" %sys-lseek)
    ("off_t" (return-wrapper off-t :error-generator signal-posix-error))
  (fildes :int)
  (offset off-t)
  (whence :int))

(defwrapper ("truncate" %sys-truncate)
    ("int" (return-wrapper :int :error-generator signal-posix-error/restart))
  (path ("const char*" filename-designator))
  (length off-t))

(defwrapper ("ftruncate" %sys-ftruncate)
    ("int" (return-wrapper :int :error-generator signal-posix-error/restart))
  (fd     :int)
  (length off-t))

(defwrapper ("mmap" %sys-mmap)
    ("void*" (return-wrapper :pointer :error-generator signal-posix-error))
  (start  :pointer)
  (length size-t)
  (prot   :int)
  (flags  :int)
  (fd     :int)
  (offset off-t))

(defwrapper ("stat" %%sys-stat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (file-name ("const char*" filename-designator))
  (buf       ("struct stat*" :pointer)))

(defwrapper ("fstat" %%sys-fstat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (filedes :int)
  (buf     ("struct stat*" :pointer)))

(defwrapper ("lstat" %%sys-lstat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (file-name ("const char*" filename-designator))
  (buf       ("struct stat*" :pointer)))

(defwrapper ("pread" %sys-pread)
    ("ssize_t" (return-wrapper ssize-t :error-generator signal-posix-error/restart))
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defwrapper ("pwrite" %sys-pwrite)
    ("ssize_t" (return-wrapper ssize-t :error-generator signal-posix-error/restart))
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))


;;;-----------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-----------------------------------------------------------------------------

(defwrapper* "get_errno" :int
  ()
  "return errno;")

(defwrapper* "set_errno" :int
  ((value :int))
  "errno = value;"
  "return errno;")

(defwrapper ("strerror_r" %sys-strerror-r)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (errnum :int)
  (buf    :string)
  (buflen size-t))
