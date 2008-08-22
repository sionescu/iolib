;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.syscalls
  (:nicknames #:isys)
  (:use :iolib.base :cffi)
  (:shadow #:time)
  (:export
   ;; Conditions
   #:system-error
   #:code-of
   #:identifier-of
   #:message-of
   #:syscall-error
   #:posix-error
   #:poll-error
   #:poll-timeout

   ;; Pathname Functions
   #:native-namestring

   ;; Type Designators
   #:filename
   #:filename-designator
   #:pointer-or-nil
   #:pointer-or-nil-designator
   #:bool
   #:bool-designator

   ;; Misc
   #:repeat-decreasing-timeout
   #:repeat-upon-condition-decreasing-timeout

   ;; Return wrapper
   #:return-wrapper
   #:error-predicate-of
   #:return-filter-of
   #:error-generator-of
   #:base-type-of
   #:never-fails
   #:signal-posix-error
   #:signal-posix-error/restart
   #:return-posix-error/restart

   ;; Syscall definition
   #:defentrypoint
   #:defcfun*
   #:defsyscall
   #:defsyscall*

   ;;;--------------------------------------------------------------------------
   ;;; Syscalls
   ;;;--------------------------------------------------------------------------

   ;; Specials
   #:*default-open-mode*
   #:*environ*

   ;; Errno-related functions
   #:%sys-strerror
   #:get-errno
   #:set-errno

   ;; Memory manipulation functions
   #:%sys-memset
   #:%sys-bzero
   #:%sys-memcpy
   #:%sys-memmove

   ;; Files
   #:%sys-read
   #:%sys-write
   #:%sys-pread
   #:%sys-pwrite
   #:%sys-open
   #:%sys-creat
   #:%sys-pipe
   #:%sys-mkfifo
   #:%sys-umask
   #:%sys-lseek
   #:%sys-access
   #:%sys-truncate
   #:%sys-ftruncate
   #:%sys-rename
   #:%sys-link
   #:%sys-symlink
   #:%sys-readlink
   #:%sys-unlink
   #:%sys-chown
   #:%sys-fchown
   #:%sys-lchown
   #:%sys-chmod
   #:%sys-fchmod
   #:%sys-stat
   #:%sys-fstat
   #:%sys-lstat
   #:%sys-sync
   #:%sys-fsync
   #:%sys-mkstemp

   ;; Directories
   #:%sys-mkdir
   #:%sys-rmdir
   #:%sys-chdir
   #:%sys-fchdir
   #:%sys-getcwd
   #:%sys-mkdtemp

   ;; File descriptors
   #:%sys-close
   #:%sys-dup
   #:%sys-dup2
   #:%sys-ioctl
   #:%sys-fd-open-p

   ;; Memory mapping
   #:%sys-mmap
   #:%sys-munmap

   ;; Time
   #:%sys-usleep
   #:%sys-time
   #:%sys-gettimeofday
   #:%sys-get-monotonic-time

   ;; Environment
   #:%sys-getenv
   #:%sys-setenv
   #:%sys-unsetenv

   ;; Local info
   #:%sys-gethostname
   #:%sys-getdomainname

   ;;;--------------------------------------------------------------------------
   ;;; Foreign types and constants
   ;;;--------------------------------------------------------------------------

   ;; Primitive type sizes
   #:size-of-char
   #:size-of-int
   #:size-of-long
   #:size-of-long-long
   #:size-of-pointer
   #:size-of-short

   ;; Types
   #:size-t
   #:ssize-t
   #:pid-t
   #:gid-t
   #:uid-t
   #:off-t
   #:mode-t
   #:time-t
   #:useconds-t
   #:suseconds-t
   #:dev-t
   #:ino-t
   #:nlink-t
   #:blksize-t
   #:blkcnt-t
   #:nfds-t

   ;; OPEN()
   #:o-rdonly
   #:o-wronly
   #:o-rdwr
   #:o-creat
   #:o-excl
   #:o-trunc
   #:o-append
   #:o-noctty
   #:o-nonblock
   #:o-ndelay
   #:o-sync
   #:o-nofollow
   #:o-async

   ;; LSEEK()
   #:seek-set
   #:seek-cur
   #:seek-end

   ;; ACCESS()
   #:r-ok
   #:w-ok
   #:x-ok
   #:f-ok

   ;; STAT()
   #:s-irwxu
   #:s-irusr
   #:s-iwusr
   #:s-ixusr
   #:s-ifmt
   #:s-ififo
   #:s-ifchr
   #:s-ifdir
   #:s-ifblk
   #:s-ifreg
   #:s-ifwht
   #:s-iread
   #:s-iwrite
   #:s-iexec
   #:s-irwxg
   #:s-irgrp
   #:s-iwgrp
   #:s-ixgrp
   #:s-irwxo
   #:s-iroth
   #:s-iwoth
   #:s-ixoth
   #:s-isuid
   #:s-isgid
   #:s-isvtx
   #:s-iflnk
   #:s-ifsock
   #:path-max

   ;; MMAP()
   #:prot-none
   #:prot-read
   #:prot-write
   #:prot-exec
   #:map-shared
   #:map-private
   #:map-fixed
   #:map-failed

   ;; POLL()
   #:pollin
   #:pollrdnorm
   #:pollrdband
   #:pollpri
   #:pollout
   #:pollwrnorm
   #:pollwrband
   #:pollerr
   #:pollrdhup
   #:pollhup
   #:pollnval

   ;;; Structs

   ;; timespec
   #:timespec
   #:sec
   #:nsec

   ;; timeval
   #:timeval
   #:sec
   #:usec

   ;; stat
   #:stat
   #:dev #:stat-dev
   #:ino #:stat-ino
   #:mode #:stat-mode
   #:nlink #:stat-nlink
   #:uid #:stat-uid
   #:gid #:stat-gid
   #:rdev #:stat-rdev
   #:size #:stat-size
   #:blksize #:stat-blksize
   #:blocks #:stat-blocks
   #:atime #:stat-atime
   #:mtime #:stat-mtime
   #:ctime #:stat-ctime

   ;; pollfd
   #:pollfd
   #:fd
   #:events
   #:revents

   ;; Syscall error codes
   #:errno-values
   #:eperm #:enoent #:esrch #:eintr #:eio #:enxio #:e2big #:enoexec
   #:ebadf #:echild #:eagain #:enomem #:eacces #:efault #:ebusy #:eexist
   #:exdev #:enodev #:enotdir #:eisdir #:einval #:enfile #:emfile
   #:enotty #:efbig #:enospc #:espipe #:erofs #:emlink #:epipe #:edom
   #:erange #:edeadlk #:enametoolong #:enolck #:enosys #:enotempty
   #:echrng #:el2nsync #:el3hlt #:el3rst #:elnrng #:eunatch #:enocsi
   #:el2hlt #:ebade #:ebadr #:exfull #:enoano #:ebadrqc #:ebadslt
   #:edeadlock #:ebfont #:enostr #:enodata #:etime #:enosr #:enopkg
   #:eadv #:esrmnt #:ecomm #:edotdot #:enotuniq #:ebadfd #:elibscn
   #:elibmax #:elibexec #:eilseq #:erestart #:estrpipe #:euclean
   #:enotnam #:enavail #:eremoteio #:enomedium #:emediumtype #:estale
   #:enotblk #:etxtbsy #:eusers #:eloop #:ewouldblock #:enomsg #:eidrm
   #:eproto #:emultihop #:ebadmsg #:eoverflow #:edquot #:einprogress
   #:ealready #:eprotonosupport #:esocktnosupport #:enotsock
   #:edestaddrreq #:emsgsize #:eprototype #:enoprotoopt #:eremote
   #:enolink #:epfnosupport #:eafnosupport #:eaddrinuse #:eaddrnotavail
   #:enetdown #:enetunreach #:enetreset #:econnaborted #:econnreset
   #:eisconn #:enotconn #:eshutdown #:etoomanyrefs #:etimedout
   #:econnrefused #:ehostdown #:ehostunreach #:enonet #:enobufs
   #:eopnotsupp
   ))
