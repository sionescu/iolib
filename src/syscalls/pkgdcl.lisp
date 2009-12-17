;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.syscalls
  (:nicknames #:isys)
  (:use :iolib.base :cffi)
  (:export
   ;; Conditions
   #:iolib-condition
   #:iolib-error
   #:syscall-error
   #:make-syscall-error
   #:poll-error
   #:poll-timeout

   ;; Condition accessors
   #:code-of
   #:identifier-of
   #:message-of
   #:handle-of
   #:handle2-of
   #:event-type-of
   #:get-syscall-error-condition

   ;; Pathname Functions
   #:native-namestring

   ;; Type Designators
   #:pointer-or-nil
   #:pointer-or-nil-designator
   #:bool
   #:bool-designator

   ;; Misc
   #:repeat-upon-condition
   #:repeat-upon-eintr
   #:repeat-decreasing-timeout
   #:repeat-upon-condition-decreasing-timeout

   ;; Syscall return wrapper
   #:syscall-wrapper
   #:error-predicate-of
   #:error-location-of
   #:return-filter-of
   #:error-generator-of
   #:syscall-restart-p
   #:base-type-of
   #:never-fails
   #:signal-syscall-error
   #:signal-syscall-error-kw
   #:signal-syscall-error/restart

   ;; Syscall definition
   #:defentrypoint
   #:defcfun*
   #:defsyscall

   ;; SSTRING <-> CSTRING
   #:+cstring-path-max+
   #:cstring-to-sstring
   #:sstring-to-cstring
   #:with-cstring-to-sstring
   #:with-sstring-to-cstring

;;;--------------------------------------------------------------------------
;;; Syscalls
;;;--------------------------------------------------------------------------

   ;; Specials
   #:*default-open-mode*
   #:*environ*

   ;; Errno-related functions
   #:%sys-strerror
   #:%sys-errno

   ;; Memory manipulation functions
   #:%sys-memset
   #:%sys-bzero
   #:%sys-memcpy
   #:%sys-memmove

   ;; Files
   #:%sys-read
   #:%sys-write
   #:%sys-readv
   #:%sys-writev
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
   #:%sys-realpath
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
   #:%sys-fcntl
   #:%sys-ioctl
   #:%sys-fd-open-p

   ;; TTYs
   #:%sys-posix-openpt
   #:%sys-grantpt
   #:%sys-unlockpt
   #:%sys-ptsname

   ;; I/O Polling
   #:%sys-select
   #:%sys-fd-zero
   #:%sys-copy-fd-set
   #:%sys-fd-isset
   #:%sys-fd-clr
   #:%sys-fd-set
   #:%sys-poll
   #+linux #:%sys-epoll-create
   #+linux #:%sys-epoll-ctl
   #+linux #:%sys-epoll-wait
   #+bsd #:%sys-kqueue
   #+bsd #:%sys-kevent
   #+bsd #:%sys-ev-set

   ;; Directory walking
   #:%sys-opendir
   #-darwin #:%sys-fdopendir
   #-darwin #:%sys-dirfd
   #:%sys-closedir
   #:%sys-readdir
   #:%sys-rewinddir
   #:%sys-seekdir
   #:%sys-telldir

   ;; Memory mapping
   #:%sys-mmap
   #:%sys-munmap

   ;; Process creation and info
   #:%sys-fork
   #:%sys-execv
   #:%sys-execvp
   #:%sys-waitpid
   #:%sys-getpid
   #:%sys-getppid
   #:%sys-gettid
   #:%sys-getuid
   #:%sys-setuid
   #:%sys-geteuid
   #:%sys-seteuid
   #:%sys-getgid
   #:%sys-setgid
   #:%sys-getegid
   #:%sys-setegid
   #:%sys-setreuid
   #:%sys-setregid
   #:%sys-getpgid
   #:%sys-setpgid
   #:%sys-getpgrp
   #:%sys-setpgrp
   #:%sys-setsid
   #:%sys-getrlimit
   #:%sys-setrlimit
   #:%sys-getrusage
   #:%sys-getpriority
   #:%sys-setpriority
   #:%sys-nice

   ;; Signals
   #:%sys-kill
   #:%sys-sigaction

   ;; Time
   #:%sys-usleep
   #:%sys-time
   #:%sys-gettimeofday
   #:%sys-get-monotonic-time

   ;; Environment
   #:%sys-getenv
   #:%sys-setenv
   #:%sys-unsetenv
   #:%sys-clearenv

   ;; Local info
   #:%sys-gethostname
   #:%sys-getdomainname
   #:%sys-uname

   ;; User info
   #:%sys-getpwnam
   #:%sys-getpwuid

   ;; Group info
   #:%sys-getgrnam
   #:%sys-getgrgid

   ;; CMSG readers
   #:%sys-cmsg-space
   #:%sys-cmsg-len
   #:%sys-cmsg-firsthdr
   #:%sys-cmsg-data

;;;--------------------------------------------------------------------------
;;; Foreign types and constants
;;;--------------------------------------------------------------------------

   ;; Primitive type sizes
   #:size-of-char
   #:size-of-short
   #:size-of-int
   #:size-of-long
   #:size-of-long-long
   #:size-of-pointer

   ;; Types
   #:size-t #:size-of-size-t
   #:ssize-t #:size-of-ssize-t
   #:pid-t #:size-of-pid-t
   #:gid-t #:size-of-gid-t
   #:uid-t #:size-of-uid-t
   #:off-t #:size-of-off-t
   #:mode-t #:size-of-mode-t
   #:time-t #:size-of-time-t
   #:useconds-t #:size-of-useconds-t
   #:suseconds-t #:size-of-suseconds-t
   #:dev-t #:size-of-dev-t
   #:ino-t #:size-of-ino-t
   #:nlink-t #:size-of-nlink-t
   #:blksize-t #:size-of-blksize-t
   #:blkcnt-t #:size-of-blkcnt-t
   #:nfds-t #:size-of-nfds-t

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

   ;; READDIR()
   #:dt-unknown
   #:dt-fifo
   #:dt-chr
   #:dt-dir
   #:dt-blk
   #:dt-reg
   #:dt-lnk
   #:dt-sock
   #:dt-wht

   ;; KILL()
   #:sighup
   #:sigquit
   #:sigtrap
   #-linux #:sigemt
   #:sigkill
   #:sigbus
   #:sigsys
   #:sigpipe
   #:sigalrm
   #:sigurg
   #:sigstop
   #:sigtstp
   #:sigcont
   #:sigchld
   #:sigttin
   #:sigttou
   #:sigio
   #:sigxcpu
   #:sigxfsz
   #:sigvtalrm
   #:sigprof
   #:sigwinch
   #-linux #:siginfo
   #:sigusr1
   #:sigusr2
   #+linux #:sigrtmin
   #+linux #:sigrtmax

   ;; SIGACTION()
   #:sig-ign
   #:sig-dfl

   ;; FCNTL()
   #:f-dupfd
   #:f-getfd
   #:f-setfd
   #:f-getfl
   #:f-setfl
   #:f-getlk
   #:f-setlk
   #:f-setlkw
   #:f-getown
   #:f-setown
   #:f-rdlck
   #:f-wrlck
   #:f-unlck
   #+linux #:f-getsig
   #+linux #:f-setsig
   #+linux #:f-setlease
   #+linux #:f-getlease

   ;; MMAP()
   #:prot-none
   #:prot-read
   #:prot-write
   #:prot-exec
   #:map-shared
   #:map-private
   #:map-fixed
   #:map-failed

   ;; SELECT()
   #:fd-setsize

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

   ;; EPOLL
   #+linux #:epoll-ctl-add
   #+linux #:epoll-ctl-del
   #+linux #:epoll-ctl-mod
   #+linux #:epollin
   #+linux #:epollrdnorm
   #+linux #:epollrdband
   #+linux #:epollpri
   #+linux #:epollout
   #+linux #:epollwrnorm
   #+linux #:epollwrband
   #+linux #:epollerr
   #+linux #:epollhup
   #+linux #:epollmsg
   #+linux #:epolloneshot
   #+linux #:epollet

   ;; KEVENT
   #+bsd #:ev-add
   #+bsd #:ev-enable
   #+bsd #:ev-disable
   #+bsd #:ev-delete
   #+bsd #:ev-oneshot
   #+bsd #:ev-clear
   #+bsd #:ev-eof
   #+bsd #:ev-error
   #+bsd #:evfilt-read
   #+bsd #:evfilt-write
   #+bsd #:evfilt-aio
   #+bsd #:evfilt-vnode
   #+bsd #:evfilt-proc
   #+bsd #:evfilt-signal
   #+bsd #:evfilt-timer
   #+(and bsd (not darwin)) #:evfilt-netdev
   #+bsd #:note-delete
   #+bsd #:note-write
   #+bsd #:note-extend
   #+bsd #:note-attrib
   #+bsd #:note-link
   #+bsd #:note-rename
   #+bsd #:note-revoke
   #+bsd #:note-exit
   #+bsd #:note-fork
   #+bsd #:note-exec
   #+bsd #:note-track
   #+bsd #:note-trackerr
   #+(and bsd (not darwin)) #:note-linkup
   #+(and bsd (not darwin)) #:note-linkdown
   #+(and bsd (not darwin)) #:note-linkinv

   ;; IOCTL()
   #:fionbio
   #:fionread

   ;; GETRLIMIT()
   #:prio-process
   #:prio-pgrp
   #:prio-user
   #:rlim-infinity
   #:rusage-self
   #:rusage-children
   #:rlimit-as
   #:rlimit-core
   #:rlimit-cpu
   #:rlimit-data
   #:rlimit-fsize
   #:rlimit-memlock
   #:rlimit-nofile
   #:rlimit-nproc
   #:rlimit-rss
   #:rlimit-stack
   #+linux #:rlim-saved-max
   #+linux #:rlim-saved-cur
   #+linux #:rlimit-locks
   #+linux #:rlimit-msgqueue
   #+linux #:rlimit-nlimits
   #+linux #:rlimit-nice
   #+linux #:rlimit-rtprio
   #+linux #:rlimit-sigpending
   #+bsd #:rlimit-sbsize

;;; Structs

   ;; timespec
   #:timespec #:size-of-timespec
   #:sec
   #:nsec

   ;; timeval
   #:timeval #:size-of-timeval
   #:sec
   #:usec

   ;; sigaction
   #:sigaction #:size-of-sigaction
   #:handler

   ;; stat
   #:stat #:size-of-stat
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

   ;; fd_set
   #:fd-set #:size-of-fd-set

   ;; pollfd
   #:pollfd #:size-of-pollfd
   #:fd
   #:events
   #:revents

   ;; epoll_data
   #+linux #:epoll-data #+linux #:size-of-epoll-data
   #+linux #:ptr
   #+linux #:fd
   #+linux #:u32
   #+linux #:u64

   ;; epoll_event
   #+linux #:epoll-event #+linux #:size-of-epoll-event
   #+linux #:events
   #+linux #:data

   ;; kevent
   #+bsd #:kevent #+bsd #:size-of-kevent
   #+bsd #:ident
   #+bsd #:filter
   #+bsd #:flags
   #+bsd #:fflags
   #+bsd #:data
   #+bsd #:udata

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
