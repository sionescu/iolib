;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *UNIX foreign function definitions.
;;;

(in-package :iolib.syscalls)

;;; Needed for clock_gettime() and friends.
#+linux (load-foreign-library "librt.so")


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(defentrypoint (setf %sys-errno) (value)
  (%%sys-set-errno value))

(defentrypoint %sys-strerror (&optional (err (%sys-errno)))
  "Look up the error message string for ERRNO. (reentrant)"
  (let ((errno
         (if (keywordp err)
             (foreign-enum-value 'errno-values err)
             err)))
    (with-foreign-pointer-as-string ((buf bufsiz) 1024)
      (%sys-strerror-r errno buf bufsiz))))

(defmethod print-object ((posix-error posix-error) stream)
  (print-unreadable-object (posix-error stream :type nil :identity nil)
    (let ((code (code-of posix-error))
          (identifier (identifier-of posix-error)))
      (format stream "POSIX Error ~A code: ~S ~S"
              identifier (or code "[No code]")
              (or (%sys-strerror code) "[Can't get error string.]")))))


;;;-------------------------------------------------------------------------
;;; Memory manipulation
;;;-------------------------------------------------------------------------

(defcfun* ("memset" %sys-memset) :pointer
  (buffer :pointer)
  (value  :int)
  (length size-t))

(defentrypoint %sys-bzero (buffer length)
  (%sys-memset buffer 0 length))

(defcfun* ("memcpy" %sys-memcpy) :pointer
  (dest   :pointer)
  (src    :pointer)
  (length size-t))

(defcfun* ("memmove" %sys-memmove) :pointer
  (dest   :pointer)
  (src    :pointer)
  (length size-t))


;;;-------------------------------------------------------------------------
;;; I/O
;;;-------------------------------------------------------------------------

(defsyscall* ("read" %sys-read) ssize-t
  "Read at most COUNT bytes from FD into the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall* ("write" %sys-write) ssize-t
  "Write at most COUNT bytes to FD from the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))


;;;-------------------------------------------------------------------------
;;; Files
;;;-------------------------------------------------------------------------

(defsyscall* ("open" %%sys-open) :int
  (pathname filename-designator)
  (flags    :int)
  (mode     mode-t))

(defvar *default-open-mode* #o666)

(defentrypoint %sys-open (pathname flags &optional (mode *default-open-mode*))
  (%%sys-open pathname flags mode))

(defsyscall* ("creat" %sys-creat) :int
  (pathname filename-designator)
  (mode     mode-t))

(defsyscall ("pipe" %%sys-pipe) :int
  (filedes :pointer))

(defentrypoint %sys-pipe ()
  "Create pipe, returns two values with the new FDs."
  (with-foreign-object (filedes :int 2)
    (%%sys-pipe filedes)
    (values (mem-aref filedes :int 0)
            (mem-aref filedes :int 1))))

(defsyscall ("mkfifo" %sys-mkfifo) :int
  "Create a FIFO (named pipe)."
  (path filename-designator)
  (mode mode-t))

(defsyscall "umask" mode-t
  "Sets the umask and returns the old one"
  (new-mode mode-t))

(defsyscall ("access" %sys-access) :int
  (path  filename-designator)
  (amode :int))

(defsyscall ("rename" %sys-rename) :int
  "Rename a file."
  (old filename-designator)
  (new filename-designator))

(defsyscall ("link" %sys-link) :int
  (path1 filename-designator)
  (path2 filename-designator))

(defsyscall ("symlink" %sys-symlink) :int
  "Creates a symbolic link"
  (name1 filename-designator)
  (name2 filename-designator))

(defsyscall ("readlink" %%sys-readlink) ssize-t
  (path    filename-designator)
  (buf     :pointer)
  (bufsize size-t))

(defentrypoint %sys-readlink (path)
  "Read value of a symbolic link."
  (with-foreign-pointer (buf 4096 bufsize)
    (let ((count (%%sys-readlink path buf bufsize)))
      (values (foreign-string-to-lisp buf :count count)))))

(defsyscall ("unlink" %sys-unlink) :int
  (path filename-designator))

(defsyscall* ("chown" %sys-chown) :int
  "Change ownership of a file."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall* ("fchown" %sys-fchown) :int
  "Change ownership of an open file."
  (fd    :int)
  (owner uid-t)
  (group uid-t))

(defsyscall* ("lchown" %sys-lchown) :int
  "Change ownership of a file or symlink."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall* ("chmod" %sys-chmod) :int
  (path filename-designator)
  (mode mode-t))

(defsyscall* ("fchmod" %sys-fchmod) :int
  (fd   :int)
  (mode mode-t))

;;; STAT()

(define-c-struct-wrapper stat ())

(defconstant +stat-version-linux+ 3)

;;; If necessary for performance reasons, we can add an optional
;;; argument to this function and use that to reuse a wrapper object.
(defentrypoint funcall-stat (fn arg)
  (with-foreign-object (buf 'stat)
    (funcall fn arg buf)
    (make-instance 'stat :pointer buf)))

(defentrypoint %sys-stat (path)
  "Get information about a file."
  (funcall-stat #'%%sys-stat path))

(defentrypoint %sys-fstat (fd)
  "Get information about a file descriptor"
  (funcall-stat #'%%sys-fstat fd))

(defentrypoint %sys-lstat (path)
  "Get information about a file or symlink."
  (funcall-stat #'%%sys-lstat path))

(defsyscall ("sync" %sys-sync) :void
  "Schedule all file system buffers to be written to disk.")

(defsyscall* ("fsync" %sys-fsync) :int
  (fildes :int))

(defsyscall ("mkstemp" %%sys-mkstemp) :int
  (template filename-designator))

(defentrypoint %sys-mkstemp (&optional (template ""))
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkstemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; Directories
;;;-------------------------------------------------------------------------

(defsyscall "mkdir" :int
  "Create a directory."
  (path filename-designator)
  (mode mode-t))

(defsyscall ("rmdir" %sys-rmdir) :int
  (path filename-designator))

(defsyscall ("chdir" %sys-chdir) :int
  "Changes the current working directory"
  (path filename-designator))

(defsyscall* ("fchdir" %sys-fchdir) :int
  (fildes :int))

(defsyscall ("getcwd" %%sys-getcwd) :string
  (buf :pointer)
  (size size-t))

(defentrypoint %sys-getcwd ()
  "Returns the current working directory as a string."
  (with-foreign-pointer (buf path-max size)
    (%%sys-getcwd buf size)))

(defsyscall ("mkdtemp" %%sys-mkdtemp) :int
  (template filename-designator))

(defentrypoint %sys-mkdtemp (&optional (template ""))
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkdtemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; File Descriptors
;;;-------------------------------------------------------------------------

(defsyscall ("close" %sys-close) :int
  "Close an open file descriptor."
  (fd :int))

(defsyscall ("dup" %sys-dup) :int
  (fildes :int))

(defsyscall* ("dup2" %sys-dup2) :int
  (fildes1 :int)
  (fildes2 :int))

(defsyscall* ("ioctl" %sys-ioctl/2) :int
  (fd      :int)
  (request :int))

(defsyscall* ("ioctl" %sys-ioctl/3) :int
 (fd      :int)
 (request :int)
 (arg     :pointer))

(defentrypoint %sys-fd-open-p (fd)
  (not (minusp (%sys-fstat fd))))


;;;-------------------------------------------------------------------------
;;; File descriptor polling
;;;-------------------------------------------------------------------------

;;; FIXME: Until a way to autodetect platform features is implemented
#+(or darwin freebsd)
(defconstant pollrdhup 0)

(defsyscall ("poll" %sys-poll) :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds-t)
  (timeout :int))


;;;-------------------------------------------------------------------------
;;; Directory walking
;;;-------------------------------------------------------------------------

(defsyscall "opendir" :pointer
  "Opens a directory for listing of its contents"
  (filename filename-designator))

(defsyscall "closedir" :int
  "Closes a directory when done listing its contents"
  (dir :pointer))

(defentrypoint %sys-readdir (dir)
  "Reads an item from the listing of a directory (reentrant)"
  (with-foreign-objects ((entry 'dirent) (result :pointer))
    (%%sys-readdir-r dir entry result)
    (if (null-pointer-p (mem-ref result :pointer))
        nil
        (with-foreign-slots ((name type fileno) entry dirent)
          (values (foreign-string-to-lisp name) type fileno)))))

(defsyscall "rewinddir" :void
  "Rewinds a directory."
  (dir :pointer))

(defsyscall "seekdir" :void
  "Seeks a directory."
  (dir :pointer)
  (pos :long))

;;; FIXME: According to POSIX docs "no errors are defined" for
;;; telldir() but Linux manpages specify a possible EBADF.
(defsyscall "telldir" off-t
  "Returns the current location in a directory"
  (dir :pointer))


;;;-------------------------------------------------------------------------
;;; Memory mapping
;;;-------------------------------------------------------------------------

(defsyscall ("munmap" %sys-munmap) :int
  "Unmap pages of memory."
  (addr :pointer)
  (len  size-t))


;;;-------------------------------------------------------------------------
;;; Process creation and info
;;;-------------------------------------------------------------------------

(defsyscall ("fork" %sys-fork) pid-t
  "Create a child process.")

(defsyscall ("getpid" %sys-getpid) pid-t
  "Returns the process id of the current process")

(defsyscall ("getppid" %sys-getppid) pid-t
  "Returns the process id of the current process's parent")

(defsyscall ("getuid" %sys-getuid) uid-t
  "Get real user id of the current process.")

(defsyscall ("setuid" %sys-setuid) :int
  "Set real user id of the current process."
  (uid uid-t))

(defsyscall ("geteuid" %sys-geteuid) uid-t
  "Get effective user id of the current process.")

(defsyscall ("seteuid" %sys-seteuid) :int
  "Set effective user id of the current process."
  (uid uid-t))

(defsyscall ("getgid" %sys-getgid) gid-t
  "Get real group id of the current process.")

(defsyscall ("setgid" %sys-setgid) :int
  "Set real group id of the current process."
  (gid gid-t))

(defsyscall ("getegid" %sys-getegid) gid-t
  "Get effective group id of the current process.")

(defsyscall ("setegid" %sys-setegid) :int
  "Set effective group id of the current process."
  (gid gid-t))

(defsyscall ("setreuid" %sys-setreuid) :int
  "Set real and effective user id of the current process."
  (ruid uid-t)
  (euid uid-t))

(defsyscall ("setregid" %sys-setregid) :int
  "Set real and effective group id of the current process."
  (rgid gid-t)
  (egid gid-t))

(defsyscall ("getpgid" %sys-getpgid) pid-t
  "Get process group id of a process."
  (pid pid-t))

(defsyscall ("setpgid" %sys-setpgid) :int
  "Set process group id of a process."
  (pid  pid-t)
  (pgid pid-t))

(defsyscall ("getpgrp" %sys-getpgrp) pid-t
  "Get process group id of the current process.")

(defsyscall ("setpgrp" %sys-setpgrp) pid-t
  "Set process group id of the current process.")

(defsyscall ("setsid" %sys-setsid) pid-t
  "Create session and set process group id of the current process.")

(defentrypoint %sys-getrlimit (resource)
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (%%sys-getrlimit resource rl)
      (values cur max))))

(defentrypoint %sys-setrlimit (resource soft-limit hard-limit)
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (setf cur soft-limit
            max hard-limit)
      (%%sys-setrlimit resource rl))))

(defsyscall ("getrusage" %%sys-getrusage) :int
  (who   :int)
  (usage :pointer))

;;; TODO: it might be more convenient to return a wrapper object here
;;; instead like we do in STAT.
(defentrypoint %sys-getrusage (who)
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

(defsyscall ("getpriority" %sys-getpriority) :int
  (which :int)
  (who   :int))

(defsyscall ("setpriority" %sys-setpriority) :int
  (which :int)
  (who   :int)
  (value :int))

(defentrypoint %sys-nice (&optional (increment 0))
  "Get or set process priority."
  ;; FIXME: race condition. might need WITHOUT-INTERRUPTS on some impl.s
  (setf (%sys-errno) 0)
  (let ((r (foreign-funcall "nice" :int increment :int)))
    (if (and (= r -1) (/= (%sys-errno) 0))
        (signal-posix-error r)
        r)))


;;;-------------------------------------------------------------------------
;;; Time
;;;-------------------------------------------------------------------------

(defsyscall* ("usleep" %sys-usleep) :int
  (useconds useconds-t))

(defsyscall ("time" %%sys-time) time-t
  (tloc :pointer))

(defentrypoint %sys-time ()
  (%%sys-time (null-pointer)))

(defsyscall ("gettimeofday" %%sys-gettimeofday) :int
  (tp  :pointer)
  (tzp :pointer))

(defentrypoint %sys-gettimeofday ()
  "Return the time in seconds and microseconds."
  (with-foreign-object (tv 'timeval)
    (with-foreign-slots ((sec usec) tv timeval)
      (%%sys-gettimeofday tv (null-pointer))
      (values sec usec))))

;;; FIXME: or we can implement this through the MACH functions.
#+darwin
(progn
  (defctype kern-return-t :int)
  (defctype clock-res-t :int)
  (defctype clock-id-t :int)
  (defctype port-t :unsigned-int)         ; not sure
  (defctype clock-serv-t port)

  (defconstant kern-success 0)

  (defconstant system-clock 0)
  (defconstant calendar-clock 1)
  (defconstant realtime-clock 0)

  (defsyscall ("mach_host_self" %sys-mach-host-self) port-t)

  (defsyscall ("host_get_clock_service" %%sys-host-get-clock-service) kern-return-t
    (host port-t)
    (id clock-id-t)
    (clock-name (:pointer clock-serv-t)))

  (defentrypoint %sys-host-get-clock-service (id &optional (host (%sys-mach-host-self)))
    (with-foreign-object (clock 'clock-serv-t)
      (%%sys-host-get-clock-service host id clock)
      (mem-ref clock :int)))

  (defsyscall ("clock_get_time" %clock-get-time) kern-return-t
    (clock-serv clock-serv-t)
    (cur-time timespec))

  (defentrypoint clock-get-time (clock-service)
    (with-foreign-object (time 'timespec)
      (%clock-get-time clock-service time)
      (with-foreign-slots ((tv-sec tv-nsec) time timespec)
        (values tv-sec tv-nsec)))))

#-darwin
(progn
  (defsyscall ("clock_getres" %%sys-clock-getres) :int
    "Returns the resolution of the clock CLOCKID."
    (clockid clockid-t)
    (res     :pointer))

  (defentrypoint %sys-clock-getres (clock-id)
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-getres clock-id ts)
        (values sec nsec))))

  (defsyscall ("clock_gettime" %%sys-clock-gettime) :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint %sys-clock-gettime (clock-id)
    "Returns the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-gettime clock-id ts)
        (values sec nsec))))

  (defsyscall ("clock_settime" %%sys-clock-settime) :int
    (clockid clockid-t)
    (tp      :pointer))

  (defentrypoint %sys-clock-settime (clock-id)
    "Sets the time of the clock CLOCKID."
    (with-foreign-object (ts 'timespec)
      (with-foreign-slots ((sec nsec) ts timespec)
        (%%sys-clock-settime clock-id ts)
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

(defsyscall ("getenv" %sys-getenv) :string
  "Returns the value of an environment variable"
  (name :string))

(defsyscall ("setenv" %sys-setenv) :int
  "Changes the value of an environment variable"
  (name      :string)
  (value     :string)
  (overwrite bool-designator))

(defsyscall ("unsetenv" %sys-unsetenv) :int
  "Removes the binding of an environment variable"
  (name :string))


;;;-------------------------------------------------------------------------
;;; Hostname info
;;;-------------------------------------------------------------------------

(defsyscall ("gethostname" %%sys-gethostname) :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-gethostname ()
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%%sys-gethostname cstr size)))

(defsyscall ("getdomainname" %%sys-getdomainname) :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-getdomainname ()
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%%sys-getdomainname cstr size)))


;;;-------------------------------------------------------------------------
;;; User info
;;;-------------------------------------------------------------------------

(defcfun ("getpwuid_r" %%sys-getpwuid-r)
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
  (uid     uid-t)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defcfun ("getpwnam_r" %%sys-getpwnam-r)
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
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
  "Gets the password-entry of a user, by user id."
  (funcall-getpw #'%%sys-getpwuid-r uid))

(defentrypoint %sys-getpwnam (name)
  "Gets the password-entry of a user, by username."
  (funcall-getpw #'%%sys-getpwnam-r name))


;;;-------------------------------------------------------------------------
;;; Group info
;;;-------------------------------------------------------------------------

(defsyscall ("getgrgid_r" %%sys-getgrgid-r)
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
  (uid     uid-t)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall ("getgrnam_r" %%sys-getgrnam-r)
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
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
  "Gets a group-entry, by group id. (reentrant)"
  (funcall-getgr #'%%sys-getgrgid-r gid))

(defentrypoint %sys-getgrnam (name)
  "Gets a group-entry, by group name. (reentrant)"
  (funcall-getgr #'%%sys-getgrnam-r name))
