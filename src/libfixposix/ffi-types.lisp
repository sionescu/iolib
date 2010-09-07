;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign type definitions.
;;;

(in-package :libfixposix)

(include "libfixposix.h")



;;;-------------------------------------------------------------------------
;;; Simple POSIX types
;;;-------------------------------------------------------------------------

(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype off-t "off_t")


;;;-------------------------------------------------------------------------
;;; errno.h
;;;-------------------------------------------------------------------------

;; FIXME: :define-constants doesn't work when
;;        the values arent' defined as macros too
(cenum (errno-values :define-constants t)
 ((:e2big "E2BIG"))
 ((:eacces "EACCES"))
 ((:eaddrinuse "EADDRINUSE"))
 ((:eaddrnotavail "EADDRNOTAVAIL"))
 ((:eafnosupport "EAFNOSUPPORT"))
 ((:ealready "EALREADY"))
 ((:ebadf "EBADF"))
 ((:ebadmsg "EBADMSG"))
 ((:ebusy "EBUSY"))
 ((:ecanceled "ECANCELED"))
 ((:echild "ECHILD"))
 ((:econnaborted "ECONNABORTED"))
 ((:econnrefused "ECONNREFUSED"))
 ((:econnreset "ECONNRESET"))
 ((:edeadlk "EDEADLK"))
 ((:edestaddrreq "EDESTADDRREQ"))
 ((:edom "EDOM"))
 ((:edquot "EDQUOT"))
 ((:eexist "EEXIST"))
 ((:efault "EFAULT"))
 ((:efbig "EFBIG"))
 ((:ehostunreach "EHOSTUNREACH"))
 ((:eidrm "EIDRM"))
 ((:eilseq "EILSEQ"))
 ((:einprogress "EINPROGRESS"))
 ((:eintr "EINTR"))
 ((:einval "EINVAL"))
 ((:eio "EIO"))
 ((:eisconn "EISCONN"))
 ((:eisdir "EISDIR"))
 ((:eloop "ELOOP"))
 ((:emfile "EMFILE"))
 ((:emlink "EMLINK"))
 ((:emsgsize "EMSGSIZE"))
 ((:emultihop "EMULTIHOP"))
 ((:enametoolong "ENAMETOOLONG"))
 ((:enetdown "ENETDOWN"))
 ((:enetreset "ENETRESET"))
 ((:enetunreach "ENETUNREACH"))
 ((:enfile "ENFILE"))
 ((:enobufs "ENOBUFS"))
 ((:enodata "ENODATA"))
 ((:enodev "ENODEV"))
 ((:enoent "ENOENT"))
 ((:enoexec "ENOEXEC"))
 ((:enolck "ENOLCK"))
 ((:enolink "ENOLINK"))
 ((:enomem "ENOMEM"))
 ((:enomsg "ENOMSG"))
 ((:enoprotoopt "ENOPROTOOPT"))
 ((:enospc "ENOSPC"))
 ((:enosr "ENOSR"))
 ((:enostr "ENOSTR"))
 ((:enosys "ENOSYS"))
 ((:enotconn "ENOTCONN"))
 ((:enotdir "ENOTDIR"))
 ((:enotempty "ENOTEMPTY"))
 ((:enotsock "ENOTSOCK"))
 ((:enotsup "ENOTSUP"))
 ((:enotty "ENOTTY"))
 ((:enxio "ENXIO"))
 ((:eopnotsupp "EOPNOTSUPP"))
 ((:eoverflow "EOVERFLOW"))
 ((:eperm "EPERM"))
 ((:epipe "EPIPE"))
 ((:eproto "EPROTO"))
 ((:eprotonosupport "EPROTONOSUPPORT"))
 ((:eprototype "EPROTOTYPE"))
 ((:erange "ERANGE"))
 ((:erofs "EROFS"))
 ((:espipe "ESPIPE"))
 ((:esrch "ESRCH"))
 ((:estale "ESTALE"))
 ((:etime "ETIME"))
 ((:etimedout "ETIMEDOUT"))
 ((:etxtbsy "ETXTBSY"))
 ((:ewouldblock "EWOULDBLOCK"))
 ((:exdev "EXDEV"))
 ;; ((:ebug "LFP_EBUG"))
 )


;;;-------------------------------------------------------------------------
;;; sys/wait.h
;;;-------------------------------------------------------------------------

(constant (wnohang "WNOHANG"))
(constant (wuntraced "WUNTRACED"))
(constant (wcontinued "WCONTINUED"))
