;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :libfixposix
  (:nicknames :lfp)
  (:use :common-lisp :alexandria :cffi)
  (:shadow #:open #:close #:read #:write #:listen
           #:truncate #:ftruncate #:time)
  (:export

   ;;;----------------------------------------------------------------------
   ;;; C Types
   ;;;----------------------------------------------------------------------

   ;; Primitive type sizes
   #:size-of-char
   #:size-of-short
   #:size-of-int
   #:size-of-long
   #:size-of-long-long
   #:size-of-pointer

   ;; POSIX Types
   #:bool #:size-of-bool
   #:size-t #:size-of-size-t
   #:ssize-t #:size-of-ssize-t
   #:off-t #:size-of-off-t
   #:time-t #:size-of-time-t
   #:suseconds-t #:size-of-suseconds-t

   ;;;----------------------------------------------------------------------
   ;;; Struct definitions, slots and accessors
   ;;;----------------------------------------------------------------------

   ;; timeval
   #:timeval #:size-of-timeval
   #:sec #:usec


   ;;;----------------------------------------------------------------------
   ;;; C Constants
   ;;;----------------------------------------------------------------------

   ;; errno.h
   #:errno-values
   #:e2big #:eacces #:eaddrinuse #:eaddrnotavail
   #:eafnosupport #:ealready #:ebadf #:ebadmsg #:ebusy #:ecanceled
   #:echild #:econnaborted #:econnrefused #:econnreset #:edeadlk
   #:edestaddrreq #:edom #:edquot #:eexist #:efault #:efbig #:ehostdown
   #:ehostunreach #:eidrm #:eilseq #:einprogress #:eintr #:einval #:eio
   #:eisconn #:eisdir #:eloop #:emfile #:emlink #:emsgsize #:emultihop
   #:enametoolong #:enetdown #:enetreset #:enetunreach #:enfile
   #:enobufs #:enodata #:enodev #:enoent #:enoexec #:enolck #:enolink
   #:enomem #:enomsg #:enonet #:enoprotoopt #:enospc #:enosr #:enostr
   #:enosys #:enotconn #:enotdir #:enotempty #:enotsock #:enotsup #:enotty
   #:enxio #:eopnotsupp #:eoverflow #:eperm #:epipe #:eproto
   #:eprotonosupport #:eprototype #:erange #:erofs #:eshutdown #:espipe
   #:esrch #:estale #:etime #:etimedout #:etxtbsy #:ewouldblock #:exdev
   #:ebug

   ;; Select()
   #:fd-setsize

   ;; Waitpid()
   #:wnohang
   #:wuntraced
   #:wcontinued


   ;;;----------------------------------------------------------------------
   ;;; Syscalls
   ;;;----------------------------------------------------------------------

   ;; Errno-related functions
   #:errno

   ;; Memory manipulation functions
   #:memset
   #:bzero
   #:memcpy
   #:memmove

   ;; I/O Polling
   #:select
   #:copy-fd-set
   #:fd-clr
   #:fd-isset
   #:fd-set
   #:fd-zero

   ;; Signals
   #:wifexited
   #:wexitstatus
   #:wifsignaled
   #:wtermsig
   #:wcoredump
   #:wifstopped
   #:wstopsig
   #:wifcontinued

   ;; CMSG readers
   #:cmsg.firsthdr
   #:cmsg.nxthdr
   #:cmsg.align
   #:cmsg.space
   #:cmsg.len
   #:cmsg.data
   ))
