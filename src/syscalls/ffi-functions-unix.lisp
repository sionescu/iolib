;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
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

(defentrypoint (setf errno) (value)
  "Set errno value."
  (%set-errno value))

(defsyscall (%strerror-r (#+linux "__xpg_strerror_r" "strerror_r"))
    :int
  (errnum :int)
  (buf    :pointer)
  (buflen size-t))

(defentrypoint strerror (&optional (err (errno)))
  "Look up the error message string for ERRNO (reentrant)."
  (let ((errno
         (if (keywordp err)
             (foreign-enum-value 'errno-values err)
             err)))
    (with-foreign-pointer-as-string ((buf bufsiz) 1024)
      (%strerror-r errno buf bufsiz))))

(defmethod print-object ((e syscall-error) s)
  (with-slots (syscall code identifier message handle handle2) e
    (if message
        (format s "~A" message)
        (print-unreadable-object (e s :type nil :identity nil)
          (format s "Syscall ~S signalled error ~A(~S) ~S"
                  syscall identifier (or code "[No code]")
                  (or (strerror code) "[Can't get error string.]"))
          (when handle (format s " FD=~A" handle))
          (when handle2 (format s " FD2=~A" handle2))))))


;;;-------------------------------------------------------------------------
;;; Memory manipulation
;;;-------------------------------------------------------------------------

(defcfun* (memset "memset") :pointer
  "Fill the first COUNT bytes of BUFFER with the constant VALUE."
  (buffer :pointer)
  (value  :int)
  (count  size-t))

(defentrypoint bzero (buffer count)
  "Fill the first COUNT bytes of BUFFER with zeros."
  (memset buffer 0 count))

(defcfun* (memcpy "memcpy") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas must not overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))

(defcfun* (memmove "memmove") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas may overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))


;;;-------------------------------------------------------------------------
;;; I/O
;;;-------------------------------------------------------------------------

(defsyscall (read "read")
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD into the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (write "write")
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD from the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (readv "readv")
    (ssize-t :restart t :handle fd)
  "Read from FD into the first IOVCNT buffers of the IOV array."
  (fd     :int)
  (iov    :pointer)
  (iovcnt :int))

(defsyscall (writev "writev")
    (ssize-t :restart t :handle fd)
  "Writes to FD the first IOVCNT buffers of the IOV array."
  (fd     :int)
  (iov    :pointer)
  (iovcnt :int))

(defsyscall (pread (#+linux "pread64" "pread"))
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD at offset OFFSET into the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defsyscall (pwrite (#+linux "pwrite64" "pwrite"))
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD at offset OFFSET from the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))


;;;-------------------------------------------------------------------------
;;; Files
;;;-------------------------------------------------------------------------

(defsyscall (%open (#+linux "open64" "open"))
    (:int :restart t)
  (path  sstring)
  (flags :int)
  (mode  mode-t))

(defvar *default-open-mode* #o666)

(defentrypoint open (path flags &optional (mode *default-open-mode*))
  "Open a file descriptor for PATH using FLAGS and permissions MODE
\(default value is *DEFAULT-OPEN-MODE* - #o666)."
  (%open path flags mode))

(defsyscall (creat (#+linux "creat64" "creat"))
    (:int :restart t)
  "Create file PATH with permissions MODE and return the new FD."
  (path sstring)
  (mode mode-t))

(defsyscall (%pipe "pipe") :int
  (fds :pointer))

(defentrypoint pipe ()
  "Create pipe, returns two values with the new FDs."
  (with-foreign-object (fds :int 2)
    (%pipe fds)
    (values (mem-aref fds :int 0)
            (mem-aref fds :int 1))))

(defsyscall (mkfifo "mkfifo") :int
  "Create a FIFO (named pipe) with name PATH and permissions MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (umask "umask") mode-t
  "Sets the umask to NEW-MODE and returns the old one."
  (new-mode mode-t))

(defsyscall (lseek (#+linux "lseek64" "lseek"))
    (off-t :handle fd)
  "Reposition the offset of the open file associated with the file descriptor FD
to the argument OFFSET according to the directive WHENCE."
  (fd     :int)
  (offset off-t)
  (whence :int))

(defsyscall (access "access") :int
  "Check whether the file PATH can be accessed using mode MODE."
  (path sstring)
  (mode :int))

(defsyscall (truncate (#+linux "truncate64" "truncate"))
    (:int :restart t)
  "Truncate the file PATH to a size of precisely LENGTH octets."
  (path   sstring)
  (length off-t))

(defsyscall (ftruncate (#+linux "ftruncate64" "ftruncate"))
    (:int :restart t :handle fd)
  "Truncate the file referenced by FD to a size of precisely LENGTH octets."
  (fd     :int)
  (length off-t))

(defsyscall (rename "rename") :int
  "Rename file named by OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (link "link") :int
  "Create a hard link from file OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (symlink "symlink") :int
  "Create a symbolic link from file OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (%readlink "readlink") ssize-t
  (path    sstring)
  (buf     :pointer)
  (bufsize size-t))

(defentrypoint readlink (path)
  "Read the file name pointed by the symbolic link PATH."
  (with-foreign-pointer (buf +cstring-path-max+ bufsize)
    (let ((count (%readlink path buf bufsize)))
      (cstring-to-sstring buf count))))

(defsyscall (%realpath "realpath") sstring
  (path          sstring)
  (resolved-path :pointer))

(defentrypoint realpath (path)
  "Read the file name pointed by the symbolic link PATH."
  (with-foreign-pointer (buf +cstring-path-max+)
    (%realpath path buf)))

(defsyscall (unlink "unlink") :int
  "Delete the file PATH from the file system."
  (path sstring))

(defsyscall (chown "chown")
    (:int :restart t)
  "Change ownership of file PATH to uid OWNER and gid GROUP(dereferences symlinks)."
  (path  sstring)
  (owner uid-t)
  (group uid-t))

(defsyscall (fchown "fchown")
    (:int :restart t :handle fd)
  "Change ownership of an open file referenced by FD to uid OWNER and gid GROUP."
  (fd    :int)
  (owner uid-t)
  (group uid-t))

(defsyscall (lchown "lchown")
    (:int :restart t)
  "Change ownership of a file PATH to uid OWNER and gid GROUP(does not dereference symlinks)."
  (path  sstring)
  (owner uid-t)
  (group uid-t))

(defsyscall (chmod "chmod")
    (:int :restart t)
  "Change permissions of file PATH to mode MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (fchmod "fchmod")
    (:int :restart t :handle fd)
  "Change permissions of open file referenced by FD to mode MODE."
  (fd   :int)
  (mode mode-t))


;;;-------------------------------------------------------------------------
;;; Stat()
;;;-------------------------------------------------------------------------

(define-c-struct-wrapper stat ())

(defsyscall (%stat (#+linux "__xstat64" "stat"))
    :int
  #+linux
  (version   :int)
  (file-name sstring)
  (buf       :pointer))

(defsyscall (%fstat (#+linux "__fxstat64" "fstat"))
    (:int :handle fd)
  #+linux
  (version :int)
  (fd      :int)
  (buf     :pointer))

(defsyscall (%lstat (#+linux "__lxstat64" "lstat"))
    :int
  #+linux
  (version   :int)
  (file-name sstring)
  (buf       :pointer))

;;; If necessary for performance reasons, we can add an optional
;;; argument to this function and use that to reuse a wrapper object.
(defentrypoint funcall-stat (fn arg)
  (with-foreign-object (buf 'stat)
    (funcall fn #+linux +stat-version+ arg buf)
    (make-instance 'stat :pointer buf)))

(defentrypoint stat (path)
  "Get information about file PATH(dereferences symlinks)."
  (funcall-stat #'%stat path))

(defentrypoint fstat (fd)
  "Get information about file descriptor FD."
  (funcall-stat #'%fstat fd))

(defentrypoint lstat (path)
  "Get information about file PATH(does not dereference symlinks)."
  (funcall-stat #'%lstat path))

(defsyscall (sync "sync") :void
  "Schedule all file system buffers to be written to disk.")

(defsyscall (fsync "fsync")
    (:int :restart t)
  "Schedule a file's buffers to be written to disk."
  (fd :int))

(defsyscall (%mkstemp (#+linux "mkstemp64" "mkstemp")) :int
  (template :pointer))

(defentrypoint mkstemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE.
Return two values: the file descriptor and the path of the temporary file."
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-sstring-to-cstring (ptr template)
      (values (%mkstemp ptr) (cstring-to-sstring ptr)))))


;;;-------------------------------------------------------------------------
;;; Directories
;;;-------------------------------------------------------------------------

(defsyscall (mkdir "mkdir") :int
  "Create directory PATH with permissions MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (rmdir "rmdir") :int
  "Delete directory PATH."
  (path sstring))

(defsyscall (chdir "chdir") :int
  "Change the current working directory to PATH."
  (path sstring))

(defsyscall (fchdir "fchdir")
    (:int :restart t :handle fd)
  "Change the current working directory to the directory referenced by FD."
  (fd :int))

(defsyscall (%getcwd "getcwd") :pointer
  (buf :pointer)
  (size size-t))

(defentrypoint getcwd ()
  "Return the current working directory as a string."
  (with-cstring-to-sstring (buf +cstring-path-max+ bufsize)
    (%getcwd buf bufsize)))

(defsyscall (%mkdtemp "mkdtemp") sstring
  (template sstring))

(defentrypoint mkdtemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE."
  (let ((template (concatenate 'string template "XXXXXX")))
    (%mkdtemp template)))


;;;-------------------------------------------------------------------------
;;; File Descriptors
;;;-------------------------------------------------------------------------

(defsyscall (close "close")
    (:int :handle fd)
  "Close open file descriptor FD."
  (fd :int))

(defsyscall (dup "dup")
    (:int :handle fd)
  "Duplicate file descriptor FD."
  (fd :int))

(defsyscall (dup2 "dup2")
    (:int :restart t :handle oldfd :handle2 newfd)
  "Make NEWFD be the copy of OLDFD, closing NEWFD first if necessary."
  (oldfd :int)
  (newfd :int))

(defsyscall (%fcntl/noarg "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int))

;;; FIXME: Linux/glibc says ARG's type is long, POSIX says it's int.
;;; Is this an issue?
(defsyscall (%fcntl/int "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :int))

(defsyscall (%fcntl/pointer "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :pointer))

(defentrypoint fcntl (fd cmd &optional (arg nil argp))
  (cond
    ((not argp)     (%fcntl/noarg   fd cmd))
    ((integerp arg) (%fcntl/int     fd cmd arg))
    ((pointerp arg) (%fcntl/pointer fd cmd arg))
    (t (error 'type-error :datum arg
              :expected-type '(or integer foreign-pointer)))))

(defentrypoint fd-nonblock (fd)
  (let ((current-flags (fcntl fd f-getfl)))
    (logtest o-nonblock current-flags)))

(defentrypoint (setf fd-nonblock) (newmode fd)
  (let* ((current-flags (fcntl fd f-getfl))
         (new-flags (if newmode
                        (logior current-flags o-nonblock)
                        (logandc2 current-flags o-nonblock))))
    (when (/= new-flags current-flags)
      (fcntl fd f-setfl new-flags))
    newmode))

(defsyscall (%ioctl/noarg "ioctl")
    (:int :handle fd)
  "Send request REQUEST to file referenced by FD."
  (fd      :int)
  (request :unsigned-int))

(defsyscall (%ioctl/pointer "ioctl")
    (:int :handle fd)
  "Send request REQUEST to file referenced by FD using argument ARG."
 (fd      :int)
 (request :unsigned-int)
 (arg     :pointer))

(defentrypoint ioctl (fd request &optional (arg nil argp))
  "Control an I/O device."
  (cond
    ((not argp)     (%ioctl/noarg   fd request))
    ((pointerp arg) (%ioctl/pointer fd request arg))
    (t (error 'type-error :datum arg :expected-type 'foreign-pointer))))

(defentrypoint fd-open-p (fd)
  (handler-case
      (progn (fstat fd) t)
    (ebadf () nil)))


;;;-------------------------------------------------------------------------
;;; TTYs
;;;-------------------------------------------------------------------------

(defsyscall (posix-openpt "posix_openpt") :int
  (flags :int))

(defsyscall (grantpt "grantpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (unlockpt "unlockpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (ptsname "ptsname")
    (:string :handle fd)
  (fd :int))


;;;-------------------------------------------------------------------------
;;; File descriptor polling
;;;-------------------------------------------------------------------------

(defsyscall (select "select") :int
  "Scan for I/O activity on multiple file descriptors."
  (nfds      :int)
  (readfds   :pointer)
  (writefds  :pointer)
  (exceptfds :pointer)
  (timeout   :pointer))

(defentrypoint fd-zero (fd-set)
  (bzero fd-set size-of-fd-set)
  (values fd-set))

(defentrypoint copy-fd-set (from to)
  (memcpy to from size-of-fd-set)
  (values to))

(deftype select-file-descriptor ()
  `(mod #.fd-setsize))

(defentrypoint fd-isset (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (logbitp bit-off oldval))))

(defentrypoint fd-clr (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logandc2 oldval (ash 1 bit-off)))))
  (values fd-set))

(defentrypoint fd-set (fd fd-set)
  (multiple-value-bind (byte-off bit-off) (floor fd 8)
    (let ((oldval (mem-aref fd-set :uint8 byte-off)))
      (setf (mem-aref fd-set :uint8 byte-off)
            (logior oldval (ash 1 bit-off)))))
  (values fd-set))

;;; FIXME: Until a way to autodetect platform features is implemented
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'pollrdhup)
    (defconstant pollrdhup 0)))

(defsyscall (poll "poll") :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds-t)
  (timeout :int))

#+linux
(progn
  (defsyscall (epoll-create "epoll_create") :int
    "Open an epoll file descriptor."
    (size :int))

  (defsyscall (epoll-ctl "epoll_ctl")
      (:int :handle epfd :handle2 fd)
    "Control interface for an epoll descriptor."
    (epfd  :int)
    (op    :int)
    (fd    :int)
    (event :pointer))

  (defsyscall (epoll-wait "epoll_wait")
      (:int :handle epfd)
    "Wait for an I/O event on an epoll file descriptor."
    (epfd      :int)
    (events    :pointer)
    (maxevents :int)
    (timeout   :int)))

#+bsd
(progn
  (defsyscall (kqueue "kqueue") :int
    "Open a kernel event queue.")

  (defsyscall (kevent "kevent")
      (:int :handle fd)
    "Control interface for a kernel event queue."
    (fd         :int)
    (changelist :pointer)               ; const struct kevent *
    (nchanges   :int)
    (eventlist  :pointer)               ; struct kevent *
    (nevents    :int)
    (timeout    :pointer))              ; const struct timespec *

  (defentrypoint ev-set (%kev %ident %filter %flags %fflags %data %udata)
    (with-foreign-slots ((ident filter flags fflags data udata) %kev kevent)
      (setf ident %ident filter %filter flags %flags
            fflags %fflags data %data udata %udata))))


;;;-------------------------------------------------------------------------
;;; Directory walking
;;;-------------------------------------------------------------------------

(defsyscall (opendir "opendir") :pointer
  "Open directory PATH for listing of its contents."
  (path sstring))

#-bsd
(defsyscall (fdopendir "fdopendir") :pointer
  "Open directory denoted by descriptor FD for listing of its contents."
  (fd :int))

(defsyscall (closedir "closedir") :int
  "Close directory DIR when done listing its contents."
  (dirp :pointer))

(defsyscall (%readdir-r (#+linux "readdir64_r" "readdir_r"))
    (:int
     :error-predicate plusp
     :error-location :return)
  (dirp   :pointer)
  (entry  :pointer)
  (result :pointer))

(defentrypoint readdir (dir)
  "Reads an item from the listing of directory DIR (reentrant)."
  (with-foreign-objects ((entry 'dirent) (result :pointer))
    (%readdir-r dir entry result)
    (if (null-pointer-p (mem-ref result :pointer))
        nil
        (with-foreign-slots ((name type fileno) entry dirent)
          (values (cstring-to-sstring name) type fileno)))))

(defsyscall (rewinddir "rewinddir") :void
  "Rewind directory DIR."
  (dirp :pointer))

(defsyscall (seekdir "seekdir") :void
  "Seek into directory DIR to position POS(as returned by TELLDIR)."
  (dirp :pointer)
  (pos  :long))

;;; FIXME: According to POSIX docs "no errors are defined" for
;;; telldir() but Linux manpages specify a possible EBADF.
(defsyscall (telldir "telldir") off-t
  "Return the current location in directory DIR."
  (dirp :pointer))


;;;-------------------------------------------------------------------------
;;; Memory mapping
;;;-------------------------------------------------------------------------

(defsyscall (mmap (#+linux "mmap64" "mmap"))
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

(defsyscall (munmap "munmap") :int
  "Unmap pages of memory starting at address ADDR with length LENGTH."
  (addr   :pointer)
  (length size-t))


;;;-------------------------------------------------------------------------
;;; Process creation and info
;;;-------------------------------------------------------------------------

(defsyscall (fork "fork") pid-t
  "Create a child process.")

(defsyscall (execv "execv") :int
  (path :string)
  (argv :pointer))

(defsyscall (execvp "execvp") :int
  (file :string)
  (argv :pointer))

(defsyscall (%waitpid "waitpid") pid-t
  (pid     pid-t)
  (status  :pointer)
  (options :int))

(defentrypoint waitpid (pid options)
  (with-foreign-pointer (status size-of-int)
    (let ((ret (%waitpid pid status options)))
      (values ret (mem-ref status :int)))))

(defsyscall (getpid "getpid") pid-t
  "Returns the process id of the current process")

(defsyscall (getppid "getppid") pid-t
  "Returns the process id of the current process's parent")

#+linux
(defentrypoint gettid ()
  (foreign-funcall "syscall" :int sys-gettid :int))

(defsyscall (getuid "getuid") uid-t
  "Get real user id of the current process.")

(defsyscall (setuid "setuid") :int
  "Set real user id of the current process to UID."
  (uid uid-t))

(defsyscall (geteuid "geteuid") uid-t
  "Get effective user id of the current process.")

(defsyscall (seteuid "seteuid") :int
  "Set effective user id of the current process to UID."
  (uid uid-t))

(defsyscall (getgid "getgid") gid-t
  "Get real group id of the current process.")

(defsyscall (setgid "setgid") :int
  "Set real group id of the current process to GID."
  (gid gid-t))

(defsyscall (getegid "getegid") gid-t
  "Get effective group id of the current process.")

(defsyscall (setegid "setegid") :int
  "Set effective group id of the current process to GID."
  (gid gid-t))

(defsyscall (setreuid "setreuid") :int
  "Set real and effective user id of the current process to RUID and EUID."
  (ruid uid-t)
  (euid uid-t))

(defsyscall (setregid "setregid") :int
  "Set real and effective group id of the current process to RGID and EGID."
  (rgid gid-t)
  (egid gid-t))

(defsyscall (getpgid "getpgid") pid-t
  "Get process group id of process PID."
  (pid pid-t))

(defsyscall (setpgid "setpgid") :int
  "Set process group id of process PID to value PGID."
  (pid  pid-t)
  (pgid pid-t))

(defsyscall (getpgrp "getpgrp") pid-t
  "Get process group id of the current process.")

(defsyscall (setpgrp "setpgrp") pid-t
  "Set process group id of the current process.")

(defsyscall (setsid "setsid") pid-t
  "Create session and set process group id of the current process.")

(defsyscall (%getrlimit (#+linux "getrlimit64" "getrlimit"))
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint getrlimit (resource)
  "Return soft and hard limit of system resource RESOURCE."
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (%getrlimit resource rl)
      (values cur max))))

(defsyscall (%setrlimit (#+linux "setrlimit64" "setrlimit"))
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint setrlimit (resource soft-limit hard-limit)
  "Set SOFT-LIMIT and HARD-LIMIT of system resource RESOURCE."
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (setf cur soft-limit
            max hard-limit)
      (%setrlimit resource rl))))

(defsyscall (%getrusage "getrusage") :int
  (who   :int)
  (usage :pointer))

;;; TODO: it might be more convenient to return a wrapper object here
;;; instead like we do in STAT.
(defentrypoint getrusage (who)
  "Return resource usage measures of WHO."
  (with-foreign-object (ru 'rusage)
    (%getrusage who ru)
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

(defsyscall (getpriority "getpriority") :int
  "Get the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO."
  (which :int)
  (who   :int))

(defsyscall (setpriority "setpriority") :int
  "Set the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO to VALUE."
  (which :int)
  (who   :int)
  (value :int))

(defentrypoint nice (&optional (increment 0))
  "Get or set process priority."
  ;; FIXME: race condition. might need WITHOUT-INTERRUPTS on some impl.s
  (setf (errno) 0)
  (let ((retval (foreign-funcall "nice" :int increment :int))
        (errno (errno)))
    (if (and (= retval -1) (/= errno 0))
        (signal-syscall-error errno "nice")
        retval)))

(defsyscall (exit "_exit") :void
  "terminate the calling process"
  (status :int))



;;;-------------------------------------------------------------------------
;;; Signals
;;;-------------------------------------------------------------------------

(defsyscall (kill "kill") :int
  "Send signal SIG to process PID."
  (pid    pid-t)
  (signum :int))

(defsyscall (sigaction "sigaction") :int
  (signum :int)
  (act    :pointer)
  (oldact :pointer))


;;;-------------------------------------------------------------------------
;;; Time
;;;-------------------------------------------------------------------------

(defsyscall (usleep "usleep") :int
  "Suspend execution for USECONDS microseconds."
  (useconds useconds-t))

(defsyscall (%time "time") time-t
  (tloc :pointer))

(defentrypoint time ()
  "Get time in seconds."
  (%time (null-pointer)))

(defsyscall (%gettimeofday "gettimeofday") :int
  (tp  :pointer)
  (tzp :pointer))

(defentrypoint gettimeofday ()
  "Return the time in seconds and microseconds."
  (with-foreign-object (tv 'timeval)
    (with-foreign-slots ((sec usec) tv timeval)
      (%gettimeofday tv (null-pointer))
      (values sec usec))))

#-darwin
(progn
  (defsyscall (%clock-getres "clock_getres") :int
    "Returns the resolution of the clock CLOCKID."
    (clockid clockid-t)
    (res     :pointer))

  (defentrypoint clock-getres (clock-id)
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%clock-getres clock-id ts)
        (values sec nsec))))

  (defsyscall (%clock-gettime "clock_gettime") :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint clock-gettime (clock-id)
    "Returns the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%clock-gettime clock-id ts)
        (values sec nsec))))

  (defsyscall (%clock-settime "clock_settime") :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint clock-settime (clock-id)
    "Sets the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%clock-settime clock-id ts)
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

  (defsyscall (mach-host-self "mach_host_self") port-t)

  (defsyscall (%host-get-clock-service "host_get_clock_service") kern-return-t
    (host       port-t)
    (id         clock-id-t)
    (clock-name :pointer))

  (defentrypoint host-get-clock-service (id &optional (host (mach-host-self)))
    (with-foreign-object (clock 'clock-serv-t)
      (%host-get-clock-service host id clock)
      (mem-ref clock :int)))

  (defsyscall (%clock-get-time "clock_get_time") kern-return-t
    (clock-serv clock-serv-t)
    (cur-time   timespec))

  (defentrypoint clock-get-time (clock-service)
    (with-foreign-object (time 'timespec)
      (%clock-get-time clock-service time)
      (with-foreign-slots ((sec nsec) time timespec)
        (values sec nsec)))))

(defentrypoint get-monotonic-time ()
  "Gets current time in seconds from a system's monotonic clock."
  (multiple-value-bind (seconds nanoseconds)
      #-darwin (clock-gettime clock-monotonic)
      #+darwin (clock-get-time (host-get-clock-service system-clock))
    (+ seconds (/ nanoseconds 1d9))))


;;;-------------------------------------------------------------------------
;;; Environment
;;;-------------------------------------------------------------------------

(defcvar ("environ" :read-only t) (:pointer :string))

(defentrypoint getenv (name)
  "Returns the value of environment variable NAME."
  (when (and (pointerp name) (null-pointer-p name))
    (setf (errno) einval)
    (signal-syscall-error einval "getenv"))
  (foreign-funcall "getenv" :string name :string))

(defsyscall (setenv "setenv") :int
  "Changes the value of environment variable NAME to VALUE.
The environment variable is overwritten only if overwrite is not NIL."
  (name      :string)
  (value     :string)
  (overwrite bool-designator))

(defsyscall (unsetenv "unsetenv") :int
  "Removes the binding of environment variable NAME."
  (name :string))

(defentrypoint clearenv ()
  "Remove all name-value pairs from the environment and set the external
variable *environ* to NULL."
  (let ((envptr *environ*))
    (unless (null-pointer-p envptr)
      (loop :for i :from 0 :by 1
            :for string := (mem-aref envptr :string i)
            :for name := (subseq string 0 (position #\= string))
            :while name :do (unsetenv name))
      (setf (mem-ref envptr :pointer) (null-pointer)))
    (values)))


;;;-------------------------------------------------------------------------
;;; Hostname info
;;;-------------------------------------------------------------------------

(defsyscall (%gethostname "gethostname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint gethostname ()
  "Return the host name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%gethostname cstr size)))

(defsyscall (%getdomainname "getdomainname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint getdomainname ()
  "Return the domain name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%getdomainname cstr size)))

(defsyscall (%uname "uname") :int
  (buf :pointer))

(defentrypoint uname ()
  "Get name and information about current kernel."
  (with-foreign-object (buf 'utsname)
    (bzero buf size-of-utsname)
    (%uname buf)
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

(defsyscall (%getpwuid-r "getpwuid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%getpwnam-r "getpwnam_r")
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
    (with-foreign-pointer (buf +cstring-path-max+ bufsize)
      (with-foreign-slots ((name passwd uid gid gecos dir shell) pw passwd-entry)
        (funcall fn arg pw buf bufsize pwp)
        (if (null-pointer-p (mem-ref pwp :pointer))
            nil
            (values name passwd uid gid gecos dir shell))))))

(defentrypoint getpwuid (uid)
  "Gets the password-entry of a user, by user id (reentrant)."
  (funcall-getpw #'%getpwuid-r uid))

(defentrypoint getpwnam (name)
  "Gets the password-entry of a user, by username (reentrant)."
  (funcall-getpw #'%getpwnam-r name))


;;;-------------------------------------------------------------------------
;;; Group info
;;;-------------------------------------------------------------------------

(defsyscall (%getgrgid-r "getgrgid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%getgrnam-r "getgrnam_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (name    :string)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

;; FIXME: return group members too
(defun funcall-getgr (fn arg)
  (with-foreign-objects ((gr 'group-entry) (grp :pointer))
    (with-foreign-pointer (buf +cstring-path-max+ bufsize)
      (with-foreign-slots ((name passwd gid) gr group-entry)
        (funcall fn arg gr buf bufsize grp)
        (if (null-pointer-p (mem-ref grp :pointer))
            nil
            (values name passwd gid))))))

(defentrypoint getgrgid (gid)
  "Gets a group-entry, by group id (reentrant)."
  (funcall-getgr #'%getgrgid-r gid))

(defentrypoint getgrnam (name)
  "Gets a group-entry, by group name (reentrant)."
  (funcall-getgr #'%getgrnam-r name))


;;;-------------------------------------------------------------------------
;;; Sysconf
;;;-------------------------------------------------------------------------

(defcfun (sysconf "sysconf") :long
  (name :int))
