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

(include "string.h" "errno.h" "sys/types.h" "sys/stat.h"
         "unistd.h" "sys/mman.h" "dirent.h" "sys/time.h"
         "sys/resource.h")


;;;-------------------------------------------------------------------------
;;; Large-file support
;;;-------------------------------------------------------------------------

;;; FIXME: this is only necessary on Linux right?

(declaim (inline %sys-lseek))
(defwrapper ("lseek" %sys-lseek)
    ("off_t" (return-wrapper off-t :error-generator signal-posix-error))
  (fildes :int)
  (offset off-t)
  (whence :int))

(declaim (inline %sys-truncate))
(defwrapper ("truncate" %sys-truncate)
    ("int" (return-wrapper :int :error-generator signal-posix-error/restart))
  (path   ("const char*" filename-designator))
  (length off-t))

(declaim (inline %sys-ftruncate))
(defwrapper ("ftruncate" %sys-ftruncate)
    ("int" (return-wrapper :int :error-generator signal-posix-error/restart))
  (fd     :int)
  (length off-t))

(declaim (inline %sys-mmap))
(defwrapper ("mmap" %sys-mmap)
    ("void*" (return-wrapper :pointer :error-generator signal-posix-error))
  (start  :pointer)
  (length size-t)
  (prot   :int)
  (flags  :int)
  (fd     :int)
  (offset off-t))

(declaim (inline %%sys-stat))
(defwrapper ("stat" %%sys-stat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (file-name ("const char*" filename-designator))
  (buf       ("struct stat*" :pointer)))

(declaim (inline %%sys-fstat))
(defwrapper ("fstat" %%sys-fstat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (filedes :int)
  (buf     ("struct stat*" :pointer)))

(declaim (inline %%sys-lstat))
(defwrapper ("lstat" %%sys-lstat)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (file-name ("const char*" filename-designator))
  (buf       ("struct stat*" :pointer)))

(declaim (inline %sys-pread))
(defwrapper ("pread" %sys-pread)
    ("ssize_t" (return-wrapper ssize-t :error-generator signal-posix-error/restart))
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(declaim (inline %sys-pwrite))
(defwrapper ("pwrite" %sys-pwrite)
    ("ssize_t" (return-wrapper ssize-t :error-generator signal-posix-error/restart))
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(declaim (inline %%sys-readdir-r))
(defwrapper ("readdir_r" %%sys-readdir-r)
    ("int" (return-wrapper :int :error-predicate (lambda (r) (not (zerop r)))
                           :error-generator signal-posix-error-from-return-value))
  (dirp   ("DIR*" :pointer))
  (entry  ("struct dirent*" :pointer))
  (result ("struct dirent**" :pointer)))


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(declaim (inline %sys-errno))
(defwrapper* ("iolib_get_errno" %sys-errno) :int
  ()
  "return errno;")

(declaim (inline %%sys-set-errno))
(defwrapper* ("iolib_set_errno" %%sys-set-errno) :int
  ((value :int))
  "errno = value;"
  "return errno;")

(declaim (inline %sys-strerror-r))
(defwrapper ("strerror_r" %sys-strerror-r)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (errnum :int)
  (buf    :string)
  (buflen size-t))


;;;-------------------------------------------------------------------------
;;; Process functions
;;;-------------------------------------------------------------------------

(declaim (inline %%sys-getrlimit))
(defwrapper ("getrlimit" %%sys-getrlimit)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (resource :int)
  (rlimit   ("struct rlimit*" :pointer)))

(declaim (inline %%sys-setrlimit))
(defwrapper ("setrlimit" %%sys-setrlimit)
    ("int" (return-wrapper :int :error-generator signal-posix-error))
  (resource :int)
  (rlimit   ("const struct rlimit*" :pointer)))
