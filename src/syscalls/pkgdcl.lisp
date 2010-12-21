;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition
;;;

(in-package :common-lisp-user)

(defpackage :iolib.syscalls
  (:nicknames #:isys)
  (:use :iolib.base :cffi)
  (:shadow #:open #:close #:read #:write #:listen
           #:truncate #:ftruncate #:time)
  ;; Specials
  (:export
   #:*environ*)
  ;; Simple POSIX Types
  (:export
   #:bool
   #:size-t
   #:ssize-t
   #:intptr-t
   #:uintptr-t
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
   #:rlim-t
   #:id-t
   #:clockid-t)

  ;;-----------------------------------------
  ;; Error conditions, wrappers and definers
  ;;-----------------------------------------
  (:export

   #:iolib-condition #:iolib-error #:syscall-error
   #:code-of #:identifier-of #:message-of #:handle-of #:handle2-of
   #:make-syscall-error #:syscall-error-p #:get-syscall-error-condition
   #:signal-syscall-error #:signal-syscall-error/restart
   #:poll-error #:event-type-of #:poll-timeout

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

   ;; Syscall definers
   #:defentrypoint
   #:defcfun*
   #:defsyscall

   ;; CFFI Type Designators
   #:pointer-or-nil
   #:pointer-or-nil-designator
   #:bool
   #:bool-designator

   ;; SSTRING <-> CSTRING
   #:+cstring-path-max+
   #:cstring-to-sstring
   #:sstring-to-cstring
   #:with-cstring-to-sstring
   #:with-sstring-to-cstring

   ;; Misc
   #:repeat-upon-condition
   #:repeat-upon-eintr
   #:repeat-decreasing-timeout
   #:repeat-upon-condition-decreasing-timeout)

  ;;--------------------------------
  ;; Structs, slots and C constants
  ;;--------------------------------
  (:export ;; errno.h
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
   #:ebug)
  (:export ;; fcntl.h
   ;; Open()
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
   #:o-cloexec
   ;; Fcntl()
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
   #+linux #:f-getlease)
  (:export ;; unistd.h
   ;; Lseek()
   #:seek-set
   #:seek-cur
   #:seek-end
   ;; Access()
   #:r-ok
   #:w-ok
   #:x-ok
   #:f-ok)
  (:export ;; time.h
   ;; struct timespec
   #:timespec
   #:sec #:nsec
   ;; Clock_gettime() et al.
   #:clock-realtime
   #:clock-monotonic)
  (:export ;; sys/stat.h
   #:path-max
   ;; struct stat
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
   ;; Stat()
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
   #:s-ifsock)
  (:export ;; sys/ioctl.h
   #:fionbio
   #:fionread)
  (:export ;; sys/wait.h
   ;; Waitpid()
   #:wnohang
   #:wuntraced
   #:wcontinued)
  (:export ;; signal.h
   ;; Kill()
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
   #:sigcld
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
   ;; struct sigaction
   #:sigaction
   #:handler
   ;; Sigaction()
   #:sig-ign
   #:sig-dfl
   #:sa-nocldstop
   #:sa-nocldwait
   #:sa-nodefer
   #:sa-onstack
   #:sa-resethand
   #:sa-restart
   #:sa-siginfo)
  (:export ;; sys/mman.h
   ;; Mmap()
   #:prot-none
   #:prot-read
   #:prot-write
   #:prot-exec
   #:map-shared
   #:map-private
   #:map-fixed
   #:map-failed)
  (:export ;; sys/select.h
   ;; fd_set
   #:fd-set
   #:fd-setsize
   ;; struct timeval
   #:timeval
   #:sec #:usec)
  (:export ;; sys/poll.h
   ;; struct pollfd
   #:pollfd
   #:fd #:events #:revents
   ;; Poll()
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
   #:pollnval)
  (:export ;; dirent.h
   ;; struct dirent
   #:dirent
   #:fileno #:type #:name
   ;; Readdir()
   #:dt-unknown
   #:dt-fifo
   #:dt-chr
   #:dt-dir
   #:dt-blk
   #:dt-reg
   #:dt-lnk
   #:dt-sock
   #:dt-wht)
  (:export ;; sys/resource.h
   ;; struct rlimit
   #:rlimit
   #:cur #:max
   ;; struct rusage
   #:rusage
   #:utime #:stime #:maxrss #:ixrss #:idrss #:isrss
   #:minflt #:majflt #:nswap #:inblock #:outblock
   #:msgsnd #:msgrcv #:nsignals #:nvcsw #:nivcsw
   ;; Getrlimit()
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
   #+bsd #:rlimit-sbsize)
  (:export ;; sys/utsname.h
   ;; struct utsname
   #:utsname
   #:sysname #:nodename #:release #:version #:machine)
  (:export ;; pwd.h
   ;; struct passwd
   #:passwd
   #:name #:passwd #:uid #:gid #:gecos #:dir #:shell)
  (:export ;; grp.h
   ;; struct group
   #:group
   #:name #:passwd #:gid #:mem)
  #+linux
  (:export ;; sys/epoll.h
   ;; union epoll_data_t
   #:epoll-data
   #:ptr #:fd #:u32 #:u64
   ;; struct epoll_event
   #:epoll-event
   #:events #:data
   ;; Epoll_ctl()
   #:epoll-ctl-add
   #:epoll-ctl-del
   #:epoll-ctl-mod
   #:epollin
   #:epollrdnorm
   #:epollrdband
   #:epollpri
   #:epollout
   #:epollwrnorm
   #:epollwrband
   #:epollerr
   #:epollhup
   #:epollmsg
   #:epolloneshot
   #:epollet)
  #+bsd
  (:export ;; sys/event.h
   ;; struct kevent
   #:kevent
   #:ident #:filter #:flags #:fflags #:data #:udata
   ;; Kevent()
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
   #-darwin #:evfilt-netdev
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
   #-darwin #:note-linkup
   #-darwin #:note-linkdown
   #-darwin #:note-linkinv)

  (:export ;; CFFI additions
   #:sizeof)

;;;----------
;;; Syscalls
;;;----------
  (:export ;; Errno-related functions
   #:errno
   #:strerror)
  (:export ;; Memory manipulation functions
   #:memset
   #:bzero
   #:memcpy
   #:memmove)
  (:export ;; Files
   #:open
   #:creat
   #:pipe
   #:mkfifo
   #:umask
   #:lseek
   #:access
   #:truncate
   #:ftruncate
   #:rename
   #:link
   #:symlink
   #:readlink
   #:realpath
   #:unlink
   #:chown
   #:fchown
   #:lchown
   #:chmod
   #:fchmod)
  (:export ;; I/O
   #:read
   #:write
   #:readv
   #:writev
   #:pread
   #:pwrite)
  (:export ;; Stat
   #:stat
   #:fstat
   #:lstat
   #:sync
   #:fsync
   #:mkstemp)
  (:export ;; Directories
   #:mkdir
   #:rmdir
   #:chdir
   #:fchdir
   #:getcwd
   #:mkdtemp)
  (:export ;; File descriptors
   #:close
   #:dup
   #:dup2
   #:fcntl
   #:ioctl
   #:fd-cloexec-p
   #:fd-nonblock-p
   #:fd-open-p)
  (:export ;; TTYs
   #:posix-openpt
   #:grantpt
   #:unlockpt
   #:ptsname)
  (:export ;; I/O Polling
   #:select
   #:copy-fd-set
   #:fd-clr
   #:fd-isset
   #:fd-set
   #:fd-zero
   #:poll
   #+linux #:epoll-create
   #+linux #:epoll-ctl
   #+linux #:epoll-wait
   #+bsd #:kqueue
   #+bsd #:kevent
   #+bsd #:ev-set)
  (:export ;; Socket message readers
   #:cmsg.firsthdr
   #:cmsg.nxthdr
   #:cmsg.space
   #:cmsg.len
   #:cmsg.data)
  (:export ;; Directory walking
   #:opendir
   #:closedir
   #:readdir
   #:rewinddir
   #:seekdir
   #:telldir)
  (:export ;; Memory mapping
   #:mmap
   #:munmap)
  (:export ;; Process creation and info
   #:waitpid
   #:getpid
   #:getppid
   #+linux #:gettid
   #:getuid
   #:setuid
   #:geteuid
   #:seteuid
   #:getgid
   #:setgid
   #:getegid
   #:setegid
   #:setreuid
   #:setregid
   #:getpgid
   #:setpgid
   #:getpgrp
   #:setpgrp
   #:setsid
   #:getrlimit
   #:setrlimit
   #:getrusage
   #:getpriority
   #:setpriority
   #:nice
   #:exit)
  (:export ;; Signals
   #:kill
   #:sigaction
   #:wifexited
   #:wexitstatus
   #:wifsignaled
   #:wtermsig
   #:wcoredump
   #:wifstopped
   #:wstopsig
   #:wifcontinued)
  (:export ;; Time
   #:usleep
   #:clock-getres
   #:clock-gettime
   #:clock-settime
   #:get-monotonic-time)
  (:export ;; Environment
   #:getenv
   #:setenv
   #:unsetenv
   #:clearenv)
  (:export ;; Hostname info
   #:gethostname
   #:getdonainname
   #:uname)
  (:export ;; User info
   #:getpwuid
   #:getpwnam)
  (:export ;; Group info
   #:getgrgid
   #:getgrnam))
