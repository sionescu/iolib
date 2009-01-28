;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *UNIX foreign function definitions.
;;;

(in-package :iolib.syscalls)

;;; Needed for clock_gettime() and friends.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library librt
    (:linux "librt.so"))
  (use-foreign-library librt))


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(defentrypoint (setf %sys-errno) (value)
  "Set errno value."
  (%%sys-set-errno value))

(defsyscall (%%sys-strerror-r (#+linux "__xpg_strerror_r" "strerror_r"))
    :int
  (errnum :int)
  (buf    :string)
  (buflen size-t))

(defentrypoint %sys-strerror (&optional (err (%sys-errno)))
  "Look up the error message string for ERRNO (reentrant)."
  (let ((errno
         (if (keywordp err)
             (foreign-enum-value 'errno-values err)
             err)))
    (with-foreign-pointer-as-string ((buf bufsiz) 1024)
      (%%sys-strerror-r errno buf bufsiz))))

(defmethod print-object ((e syscall-error) stream)
  (let ((code (code-of e))
        (identifier (identifier-of e))
        (message (message-of e)))
    (if message
        (format stream "~A" message)
        (print-unreadable-object (e stream :type nil :identity nil)
          (format stream "System-Error ~A(~S) ~S"
                  identifier (or code "[No code]")
                  (or (%sys-strerror code) "[Can't get error string.]"))))))


;;;-------------------------------------------------------------------------
;;; Memory manipulation
;;;-------------------------------------------------------------------------

(defcfun* (%sys-memset "memset") :pointer
  "Fill the first COUNT bytes of BUFFER with the constant VALUE."
  (buffer :pointer)
  (value  :int)
  (count  size-t))

(defentrypoint %sys-bzero (buffer count)
  "Fill the first COUNT bytes of BUFFER with zeros."
  (%sys-memset buffer 0 count))

(defcfun* (%sys-memcpy "memcpy") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas must not overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))

(defcfun* (%sys-memmove "memmove") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas may overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))


;;;-------------------------------------------------------------------------
;;; I/O
;;;-------------------------------------------------------------------------

(defsyscall (%sys-read "read")
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD into the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (%sys-write "write")
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD from the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (%sys-pread (#+linux "pread64" "pread"))
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD at offset OFFSET into the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defsyscall (%sys-pwrite (#+linux "pwrite64" "pwrite"))
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD at offset OFFSET from the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))


;;;-------------------------------------------------------------------------
;;; Files
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-open (#+linux "open64" "open"))
    (:int :restart t)
  (path  filename-designator)
  (flags :int)
  (mode  mode-t))

(defvar *default-open-mode* #o666)

(defentrypoint %sys-open (path flags &optional (mode *default-open-mode*))
  "Open a file descriptor for PATH using FLAGS and permissions MODE
\(default value is *DEFAULT-OPEN-MODE* - #o666)."
  (%%sys-open path flags mode))

(defsyscall (%sys-creat (#+linux "creat64" "creat"))
    (:int :restart t)
  "Create file PATH with permissions MODE and return the new FD."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%%sys-pipe "pipe") :int
  (fds :pointer))

(defentrypoint %sys-pipe ()
  "Create pipe, returns two values with the new FDs."
  (with-foreign-object (fds :int 2)
    (%%sys-pipe fds)
    (values (mem-aref fds :int 0)
            (mem-aref fds :int 1))))

(defsyscall (%sys-mkfifo "mkfifo") :int
  "Create a FIFO (named pipe) with name PATH and permissions MODE."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%sys-umask "umask") mode-t
  "Sets the umask to NEW-MODE and returns the old one."
  (new-mode mode-t))

(defsyscall (%sys-lseek (#+linux "lseek64" "lseek"))
    (off-t :handle fd)
  "Reposition the offset of the open file associated with the file descriptor FD
to the argument OFFSET according to the directive WHENCE."
  (fd     :int)
  (offset off-t)
  (whence :int))

(defsyscall (%sys-access "access") :int
  "Check whether the file PATH can be accessed using mode MODE."
  (path filename-designator)
  (mode :int))

(defsyscall (%sys-truncate (#+linux "truncate64" "truncate"))
    (:int :restart t)
  "Truncate the file PATH to a size of precisely LENGTH octets."
  (path   filename-designator)
  (length off-t))

(defsyscall (%sys-ftruncate (#+linux "ftruncate64" "ftruncate"))
    (:int :restart t :handle fd)
  "Truncate the file referenced by FD to a size of precisely LENGTH octets."
  (fd     :int)
  (length off-t))

(defsyscall (%sys-rename "rename") :int
  "Rename file named by OLDPATH to NEWPATH."
  (oldpath filename-designator)
  (newpath filename-designator))

(defsyscall (%sys-link "link") :int
  "Create a hard link from file OLDPATH to NEWPATH."
  (oldpath filename-designator)
  (newpath filename-designator))

(defsyscall (%sys-symlink "symlink") :int
  "Create a symbolic link from file OLDPATH to NEWPATH."
  (oldpath filename-designator)
  (newpath filename-designator))

(defsyscall (%%sys-readlink "readlink") ssize-t
  (path    filename-designator)
  (buf     :pointer)
  (bufsize size-t))

(defentrypoint %sys-readlink (path)
  "Read the file name pointed by the symbolic link PATH."
  (with-foreign-pointer (buf 4096 bufsize)
    (let ((count (%%sys-readlink path buf bufsize)))
      (values (foreign-string-to-lisp buf :count count)))))

(defsyscall (%sys-unlink "unlink") :int
  "Delete the file PATH from the file system."
  (path filename-designator))

(defsyscall (%sys-chown "chown")
    (:int :restart t)
  "Change ownership of file PATH to uid OWNER and gid GROUP(dereferences symlinks)."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall (%sys-fchown "fchown")
    (:int :restart t :handle fd)
  "Change ownership of an open file referenced by FD to uid OWNER and gid GROUP."
  (fd    :int)
  (owner uid-t)
  (group uid-t))

(defsyscall (%sys-lchown "lchown")
    (:int :restart t)
  "Change ownership of a file PATH to uid OWNER and gid GROUP(does not dereference symlinks)."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall (%sys-chmod "chmod")
    (:int :restart t)
  "Change permissions of file PATH to mode MODE."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%sys-fchmod "fchmod")
    (:int :restart t :handle fd)
  "Change permissions of open file referenced by FD to mode MODE."
  (fd   :int)
  (mode mode-t))

;;; STAT()

(define-c-struct-wrapper stat ())

#+linux
(defconstant +stat-version+ 3)

(defsyscall (%%sys-stat (#+linux "__xstat64" "stat"))
    :int
  #+linux
  (version   :int)
  (file-name filename-designator)
  (buf       :pointer))

(defsyscall (%%sys-fstat (#+linux "__fxstat64" "fstat"))
    (:int :handle fd)
  #+linux
  (version :int)
  (fd      :int)
  (buf     :pointer))

(defsyscall (%%sys-lstat (#+linux "__lxstat64" "lstat"))
    :int
  #+linux
  (version   :int)
  (file-name filename-designator)
  (buf       :pointer))

;;; If necessary for performance reasons, we can add an optional
;;; argument to this function and use that to reuse a wrapper object.
(defentrypoint funcall-stat (fn arg)
  (with-foreign-object (buf 'stat)
    (funcall fn #+linux +stat-version+ arg buf)
    (make-instance 'stat :pointer buf)))

(defentrypoint %sys-stat (path)
  "Get information about file PATH(dereferences symlinks)."
  (funcall-stat #'%%sys-stat path))

(defentrypoint %sys-fstat (fd)
  "Get information about file descriptor FD."
  (funcall-stat #'%%sys-fstat fd))

(defentrypoint %sys-lstat (path)
  "Get information about file PATH(does not dereference symlinks)."
  (funcall-stat #'%%sys-lstat path))

(defsyscall (%sys-sync "sync") :void
  "Schedule all file system buffers to be written to disk.")

(defsyscall (%sys-fsync "fsync")
    (:int :restart t)
  "Schedule a file's buffers to be written to disk."
  (fd :int))

(defsyscall (%%sys-mkstemp (#+linux "mkstemp64" "mkstemp")) :int
  (template filename-designator))

(defentrypoint %sys-mkstemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE."
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkstemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; Directories
;;;-------------------------------------------------------------------------

(defsyscall (%sys-mkdir "mkdir") :int
  "Create directory PATH with permissions MODE."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%sys-rmdir "rmdir") :int
  "Delete directory PATH."
  (path filename-designator))

(defsyscall (%sys-chdir "chdir") :int
  "Change the current working directory to PATH."
  (path filename-designator))

(defsyscall (%sys-fchdir "fchdir")
    (:int :restart t :handle fd)
  "Change the current working directory to the directory referenced by FD."
  (fd :int))

(defsyscall (%%sys-getcwd "getcwd") :string
  (buf :pointer)
  (size size-t))

(defentrypoint %sys-getcwd ()
  "Return the current working directory as a string."
  (with-foreign-pointer (buf path-max size)
    (%%sys-getcwd buf size)))

(defsyscall (%%sys-mkdtemp "mkdtemp") :int
  (template filename-designator))

(defentrypoint %sys-mkdtemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE."
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkdtemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; File Descriptors
;;;-------------------------------------------------------------------------

(defsyscall (%sys-close "close")
    (:int :handle fd)
  "Close open file descriptor FD."
  (fd :int))

(defsyscall (%sys-dup "dup")
    (:int :handle fd)
  "Duplicate file descriptor FD."
  (fd :int))

(defsyscall (%sys-dup2 "dup2")
    (:int :restart t :handle oldfd :handle2 newfd)
  "Make NEWFD be the copy of OLDFD, closing NEWFD first if necessary."
  (oldfd :int)
  (newfd :int))

(defsyscall (%%sys-fcntl/noarg "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int))

;;; FIXME: Linux/glibc says ARG's type is long, POSIX says it's int.
;;; Is this an issue?
(defsyscall (%%sys-fcntl/int "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :int))

(defsyscall (%%sys-fcntl/pointer "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :pointer))

(defentrypoint %sys-fcntl (fd cmd &optional (arg nil argp))
  (cond
    ((not argp)     (%%sys-fcntl/noarg   fd cmd))
    ((integerp arg) (%%sys-fcntl/int     fd cmd arg))
    ((pointerp arg) (%%sys-fcntl/pointer fd cmd arg))
    (t (error "Wrong argument to fcntl: ~S" arg))))

(defsyscall (%%sys-ioctl/noarg "ioctl")
    (:int :restart t :handle fd)
  "Send request REQUEST to file referenced by FD."
  (fd      :int)
  (request :int))

(defsyscall (%%sys-ioctl/pointer "ioctl")
    (:int :restart t :handle fd)
  "Send request REQUEST to file referenced by FD using argument ARG."
 (fd      :int)
 (request :int)
 (arg     :pointer))

(defentrypoint %sys-ioctl (fd request &optional (arg nil argp))
  "Control an I/O device."
  (cond
    ((not argp)     (%%sys-ioctl/noarg   fd request))
    ((pointerp arg) (%%sys-ioctl/pointer fd request arg))
    (t (error "Wrong argument to ioctl: ~S" arg))))

(defentrypoint %sys-fd-open-p (fd)
  (handler-case
      (progn (%sys-fstat fd) t)
    (ebadf () nil)))


;;;-------------------------------------------------------------------------
;;; TTYs
;;;-------------------------------------------------------------------------

(defsyscall (%sys-posix-openpt "posix_openpt") :int
  (flags :int))

(defsyscall (%sys-grantpt "grantpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (%sys-unlockpt "unlockpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (%sys-ptsname "ptsname")
    (:string :handle fd)
  (fd :int))


;;;-------------------------------------------------------------------------
;;; File descriptor polling
;;;-------------------------------------------------------------------------

(defsyscall (%sys-select "select") :int
  "Scan for I/O activity on multiple file descriptors."
  (nfds      :int)
  (readfds   :pointer)
  (writefds  :pointer)
  (exceptfds :pointer)
  (timeout   :pointer))

(defentrypoint %sys-fd-zero (fd-set)
  (%sys-bzero fd-set size-of-fd-set)
  (values fd-set))

(defentrypoint %sys-copy-fd-set (from to)
  (%sys-memcpy to from size-of-fd-set)
  (values to))

(deftype select-file-descriptor ()
  `(mod #.fd-setsize))

(defentrypoint %sys-fd-isset (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (logbitp bit-off oldval))))

(defentrypoint %sys-fd-clr (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logandc2 oldval (ash 1 bit-off)))))
  (values fd-set))

(defentrypoint %sys-fd-set (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logior oldval (ash 1 bit-off)))))
  (values fd-set))

;;; FIXME: Until a way to autodetect platform features is implemented
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'pollrdhup)
    (defconstant pollrdhup 0)))

(defsyscall (%sys-poll "poll") :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds-t)
  (timeout :int))

#+linux
(progn
  (defsyscall (%sys-epoll-create "epoll_create") :int
    "Open an epoll file descriptor."
    (size :int))

  (defsyscall (%sys-epoll-ctl "epoll_ctl")
      (:int :handle epfd :handle2 fd)
    "Control interface for an epoll descriptor."
    (epfd  :int)
    (op    :int)
    (fd    :int)
    (event :pointer))

  (defsyscall (%sys-epoll-wait "epoll_wait")
      (:int :handle epfd)
    "Wait for an I/O event on an epoll file descriptor."
    (epfd      :int)
    (events    :pointer)
    (maxevents :int)
    (timeout   :int)))

#+bsd
(progn
  (defsyscall (%sys-kqueue "kqueue") :int
    "Open a kernel event queue.")

  (defsyscall (%sys-kevent "kevent")
      (:int :handle fd)
    "Control interface for a kernel event queue."
    (fd         :int)
    (changelist :pointer)               ; const struct kevent *
    (nchanges   :int)
    (eventlist  :pointer)               ; struct kevent *
    (nevents    :int)
    (timeout    :pointer))              ; const struct timespec *

  (defentrypoint %sys-ev-set (%kev %ident %filter %flags %fflags %data %udata)
    (with-foreign-slots ((ident filter flags fflags data udata) %kev kevent)
      (setf ident %ident filter %filter flags %flags
            fflags %fflags data %data udata %udata))))


;;;-------------------------------------------------------------------------
;;; Directory walking
;;;-------------------------------------------------------------------------

(defsyscall (%sys-opendir "opendir") :pointer
  "Open directory PATH for listing of its contents."
  (path filename-designator))

(defsyscall (%sys-closedir "closedir") :int
  "Close directory DIR when done listing its contents."
  (dirp :pointer))

(defsyscall (%%sys-readdir-r (#+linux "readdir64_r" "readdir_r"))
    (:int
     :error-predicate plusp
     :error-location :return)
  (dirp   :pointer)
  (entry  :pointer)
  (result :pointer))

(defentrypoint %sys-readdir (dir)
  "Reads an item from the listing of directory DIR (reentrant)."
  (with-foreign-objects ((entry 'dirent) (result :pointer))
    (%%sys-readdir-r dir entry result)
    (if (null-pointer-p (mem-ref result :pointer))
        nil
        (with-foreign-slots ((name type fileno) entry dirent)
          (values (foreign-string-to-lisp name) type fileno)))))

(defsyscall (%sys-rewinddir "rewinddir") :void
  "Rewind directory DIR."
  (dirp :pointer))

(defsyscall (%sys-seekdir "seekdir") :void
  "Seek into directory DIR to position POS(as returned by TELLDIR)."
  (dirp :pointer)
  (pos  :long))

;;; FIXME: According to POSIX docs "no errors are defined" for
;;; telldir() but Linux manpages specify a possible EBADF.
(defsyscall (%sys-telldir "telldir") off-t
  "Return the current location in directory DIR."
  (dirp :pointer))


;;;-------------------------------------------------------------------------
;;; Memory mapping
;;;-------------------------------------------------------------------------

(defsyscall (%sys-mmap (#+linux "mmap64" "mmap"))
    (:pointer :handle fd)
  "Map file referenced by FD at offset OFFSET into address space of the
calling process at address ADDR and length LENGTH.
PROT describes the desired memory protection of the mapping.
FLAGS determines whether updates to the mapping are visible to other
processes mapping the same region."
  (addr   :pointer)
  (length size-t)
  (prot   :int)
  (flags  :int)
  (fd     :int)
  (offset off-t))

(defsyscall (%sys-munmap "munmap") :int
  "Unmap pages of memory starting at address ADDR with length LENGTH."
  (addr   :pointer)
  (length size-t))


;;;-------------------------------------------------------------------------
;;; Process creation and info
;;;-------------------------------------------------------------------------

(defsyscall (%sys-fork "fork") pid-t
  "Create a child process.")

(defsyscall (%sys-execv "execv") :int
  (path :string)
  (argv :pointer))

(defsyscall (%sys-execvp "execvp") :int
  (file :string)
  (argv :pointer))

(defsyscall (%sys-waitpid "waitpid") pid-t
  (pid     pid-t)
  (status  :pointer)
  (options :int))

(defsyscall (%sys-getpid "getpid") pid-t
  "Returns the process id of the current process")

(defsyscall (%sys-getppid "getppid") pid-t
  "Returns the process id of the current process's parent")

#+linux
(defentrypoint %sys-gettid ()
  (foreign-funcall "syscall" :int sys-gettid :int))

(defsyscall (%sys-getuid "getuid") uid-t
  "Get real user id of the current process.")

(defsyscall (%sys-setuid "setuid") :int
  "Set real user id of the current process to UID."
  (uid uid-t))

(defsyscall (%sys-geteuid "geteuid") uid-t
  "Get effective user id of the current process.")

(defsyscall (%sys-seteuid "seteuid") :int
  "Set effective user id of the current process to UID."
  (uid uid-t))

(defsyscall (%sys-getgid "getgid") gid-t
  "Get real group id of the current process.")

(defsyscall (%sys-setgid "setgid") :int
  "Set real group id of the current process to GID."
  (gid gid-t))

(defsyscall (%sys-getegid "getegid") gid-t
  "Get effective group id of the current process.")

(defsyscall (%sys-setegid "setegid") :int
  "Set effective group id of the current process to GID."
  (gid gid-t))

(defsyscall (%sys-setreuid "setreuid") :int
  "Set real and effective user id of the current process to RUID and EUID."
  (ruid uid-t)
  (euid uid-t))

(defsyscall (%sys-setregid "setregid") :int
  "Set real and effective group id of the current process to RGID and EGID."
  (rgid gid-t)
  (egid gid-t))

(defsyscall (%sys-getpgid "getpgid") pid-t
  "Get process group id of process PID."
  (pid pid-t))

(defsyscall (%sys-setpgid "setpgid") :int
  "Set process group id of process PID to value PGID."
  (pid  pid-t)
  (pgid pid-t))

(defsyscall (%sys-getpgrp "getpgrp") pid-t
  "Get process group id of the current process.")

(defsyscall (%sys-setpgrp "setpgrp") pid-t
  "Set process group id of the current process.")

(defsyscall (%sys-setsid "setsid") pid-t
  "Create session and set process group id of the current process.")

(defsyscall (%%sys-getrlimit (#+linux "getrlimit64" "getrlimit"))
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint %sys-getrlimit (resource)
  "Return soft and hard limit of system resource RESOURCE."
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (%%sys-getrlimit resource rl)
      (values cur max))))

(defsyscall (%%sys-setrlimit (#+linux "setrlimit64" "setrlimit"))
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint %sys-setrlimit (resource soft-limit hard-limit)
  "Set SOFT-LIMIT and HARD-LIMIT of system resource RESOURCE."
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (setf cur soft-limit
            max hard-limit)
      (%%sys-setrlimit resource rl))))

(defsyscall (%%sys-getrusage "getrusage") :int
  (who   :int)
  (usage :pointer))

;;; TODO: it might be more convenient to return a wrapper object here
;;; instead like we do in STAT.
(defentrypoint %sys-getrusage (who)
  "Return resource usage measures of WHO."
  (with-foreign-object (ru 'rusage)
    (%%sys-getrusage who ru)
    (with-foreign-slots ((maxrss ixrss idrss isrss minflt majflt nswap inblock
                          oublock msgsnd msgrcv nsignals nvcsw nivcsw)
                         ru rusage)
      (values (foreign-slot-value (foreign-slot-pointer ru 'rusage 'utime)
                                  'timeval 'sec)
              (foreign-slot-value (foreign-slot-pointer ru 'rusage 'utime)
                                  'timeval 'usec)
              (foreign-slot-value (foreign-slot-pointer ru 'rusage 'stime)
                                  'timeval 'sec)
              (foreign-slot-value (foreign-slot-pointer ru 'rusage 'stime)
                                  'timeval 'usec)
              maxrss ixrss idrss isrss minflt majflt
              nswap inblock oublock msgsnd
              msgrcv nsignals nvcsw nivcsw))))

(defsyscall (%sys-getpriority "getpriority") :int
  "Get the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO."
  (which :int)
  (who   :int))

(defsyscall (%sys-setpriority "setpriority") :int
  "Set the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO to VALUE."
  (which :int)
  (who   :int)
  (value :int))

(defentrypoint %sys-nice (&optional (increment 0))
  "Get or set process priority."
  ;; FIXME: race condition. might need WITHOUT-INTERRUPTS on some impl.s
  (setf (%sys-errno) 0)
  (let ((retval (foreign-funcall "nice" :int increment :int))
        (errno (%sys-errno)))
    (if (and (= retval -1) (/= errno 0))
        (signal-syscall-error errno)
        retval)))


;;;-------------------------------------------------------------------------
;;; Signals
;;;-------------------------------------------------------------------------

(defsyscall (%sys-kill "kill") :int
  "Send signal SIG to process PID."
  (pid    pid-t)
  (signum :int))

(defsyscall (%sys-sigaction "sigaction") :int
  (signum :int)
  (act    :pointer)
  (oldact :pointer))


;;;-------------------------------------------------------------------------
;;; Time
;;;-------------------------------------------------------------------------

(defsyscall (%sys-usleep "usleep") :int
  "Suspend execution for USECONDS microseconds."
  (useconds useconds-t))

(defsyscall (%%sys-time "time") time-t
  (tloc :pointer))

(defentrypoint %sys-time ()
  "Get time in seconds."
  (%%sys-time (null-pointer)))

(defsyscall (%%sys-gettimeofday "gettimeofday") :int
  (tp  :pointer)
  (tzp :pointer))

(defentrypoint %sys-gettimeofday ()
  "Return the time in seconds and microseconds."
  (with-foreign-object (tv 'timeval)
    (with-foreign-slots ((sec usec) tv timeval)
      (%%sys-gettimeofday tv (null-pointer))
      (values sec usec))))

#-darwin
(progn
  (defsyscall (%%sys-clock-getres "clock_getres") :int
    "Returns the resolution of the clock CLOCKID."
    (clockid clockid-t)
    (res     :pointer))

  (defentrypoint %sys-clock-getres (clock-id)
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-getres clock-id ts)
        (values sec nsec))))

  (defsyscall (%%sys-clock-gettime "clock_gettime") :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint %sys-clock-gettime (clock-id)
    "Returns the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-gettime clock-id ts)
        (values sec nsec))))

  (defsyscall (%%sys-clock-settime "clock_settime") :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint %sys-clock-settime (clock-id)
    "Sets the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-settime clock-id ts)
        (values sec nsec)))))

;;; FIXME: or we can implement this through the MACH functions.
#+darwin
(progn
  (defctype kern-return-t :int)
  (defctype clock-res-t :int)
  (defctype clock-id-t :int)
  (defctype port-t :unsigned-int)         ; not sure
  (defctype clock-serv-t port-t)

  (defconstant kern-success 0)

  (defconstant system-clock 0)
  (defconstant calendar-clock 1)
  (defconstant realtime-clock 0)

  (defsyscall (%sys-mach-host-self "mach_host_self") port-t)

  (defsyscall (%%sys-host-get-clock-service "host_get_clock_service") kern-return-t
    (host       port-t)
    (id         clock-id-t)
    (clock-name :pointer))

  (defentrypoint %sys-host-get-clock-service (id &optional (host (%sys-mach-host-self)))
    (with-foreign-object (clock 'clock-serv-t)
      (%%sys-host-get-clock-service host id clock)
      (mem-ref clock :int)))

  (defsyscall (%%sys-clock-get-time "clock_get_time") kern-return-t
    (clock-serv clock-serv-t)
    (cur-time   timespec))

  (defentrypoint %sys-clock-get-time (clock-service)
    (with-foreign-object (time 'timespec)
      (%%sys-clock-get-time clock-service time)
      (with-foreign-slots ((sec nsec) time timespec)
        (values sec nsec)))))

(defentrypoint %sys-get-monotonic-time ()
  "Gets current time in seconds from a system's monotonic clock."
  (multiple-value-bind (seconds nanoseconds)
      #-darwin (%sys-clock-gettime clock-monotonic)
      #+darwin (%sys-clock-get-time (%sys-host-get-clock-service system-clock))
    (+ seconds (/ nanoseconds 1d9))))


;;;-------------------------------------------------------------------------
;;; Environement
;;;-------------------------------------------------------------------------

(defcvar ("environ" :read-only t) (:pointer :string))

(defsyscall (%sys-getenv "getenv") :string
  "Returns the value of environment variable NAME."
  (name :string))

(defsyscall (%sys-setenv "setenv") :int
  "Changes the value of environment variable NAME to VALUE.
The environment variable is overwritten only if overwrite it non-NIL."
  (name      :string)
  (value     :string)
  (overwrite bool-designator))

(defsyscall (%sys-unsetenv "unsetenv") :int
  "Removes the binding of environment variable NAME."
  (name :string))


;;;-------------------------------------------------------------------------
;;; Hostname info
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-gethostname "gethostname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-gethostname ()
  "Return the host name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%%sys-gethostname cstr size)))

(defsyscall (%%sys-getdomainname "getdomainname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-getdomainname ()
  "Return the domain name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%%sys-getdomainname cstr size)))

(defsyscall (%%sys-uname "uname") :int
  (buf :pointer))

(defentrypoint %sys-uname ()
  "Get name and information about current kernel."
  (with-foreign-object (buf 'utsname)
    (%sys-bzero buf size-of-utsname)
    (%%sys-uname buf)
    (macrolet ((utsname-slot (name)
                 `(foreign-string-to-lisp
                   (foreign-slot-pointer buf 'utsname ',name))))
      (values (utsname-slot sysname)
              (utsname-slot nodename)
              (utsname-slot release)
              (utsname-slot version)
              (utsname-slot machine)))))


;;;-------------------------------------------------------------------------
;;; User info
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-getpwuid-r "getpwuid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%%sys-getpwnam-r "getpwnam_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (name    :string)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defun funcall-getpw (fn arg)
  (with-foreign-objects ((pw 'passwd-entry) (pwp :pointer))
    (with-foreign-pointer (buf 4096 bufsize)
      (with-foreign-slots ((name passwd uid gid gecos dir shell) pw passwd-entry)
        (funcall fn arg pw buf bufsize pwp)
        (if (null-pointer-p (mem-ref pwp :pointer))
            nil
            (values name passwd uid gid gecos dir shell))))))

(defentrypoint %sys-getpwuid (uid)
  "Gets the password-entry of a user, by user id (reentrant)."
  (funcall-getpw #'%%sys-getpwuid-r uid))

(defentrypoint %sys-getpwnam (name)
  "Gets the password-entry of a user, by username (reentrant)."
  (funcall-getpw #'%%sys-getpwnam-r name))


;;;-------------------------------------------------------------------------
;;; Group info
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-getgrgid-r "getgrgid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%%sys-getgrnam-r "getgrnam_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (name    :string)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defun funcall-getgr (fn arg)
  (with-foreign-objects ((gr 'group-entry) (grp :pointer))
    (with-foreign-pointer (buf 4096 bufsize)
      (with-foreign-slots ((name passwd gid) gr group-entry)
        (funcall fn arg gr buf bufsize grp)
        (if (null-pointer-p (mem-ref grp :pointer))
            nil
            (values name passwd gid))))))

(defentrypoint %sys-getgrgid (gid)
  "Gets a group-entry, by group id (reentrant)."
  (funcall-getgr #'%%sys-getgrgid-r gid))

(defentrypoint %sys-getgrnam (name)
  "Gets a group-entry, by group name (reentrant)."
  (funcall-getgr #'%%sys-getgrnam-r name))
