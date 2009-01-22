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
  (%%sys-set-errno value))

(defsyscall (%%sys-strerror-r (#+linux "__xpg_strerror_r" "strerror_r"))
    :int
  (errnum :int)
  (buf    :string)
  (buflen size-t))

(defentrypoint %sys-strerror (&optional (err (%sys-errno)))
  "Look up the error message string for ERRNO. (reentrant)"
  (let ((errno
         (if (keywordp err)
             (foreign-enum-value 'errno-values err)
             err)))
    (with-foreign-pointer-as-string ((buf bufsiz) 1024)
      (%%sys-strerror-r errno buf bufsiz))))

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

(defcfun* (%sys-memset "memset") :pointer
  (buffer :pointer)
  (value  :int)
  (length size-t))

(defentrypoint %sys-bzero (buffer length)
  (%sys-memset buffer 0 length))

(defcfun* (%sys-memcpy "memcpy") :pointer
  (dest   :pointer)
  (src    :pointer)
  (length size-t))

(defcfun* (%sys-memmove "memmove") :pointer
  (dest   :pointer)
  (src    :pointer)
  (length size-t))


;;;-------------------------------------------------------------------------
;;; I/O
;;;-------------------------------------------------------------------------

(defsyscall* (%sys-read "read") ssize-t
  "Read at most COUNT bytes from FD into the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall* (%sys-write "write") ssize-t
  "Write at most COUNT bytes to FD from the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall* (%sys-pread (#+linux "pread64" "pread"))
    ssize-t
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defsyscall* (%sys-pwrite (#+linux "pwrite64" "pwrite"))
    ssize-t
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))


;;;-------------------------------------------------------------------------
;;; Files
;;;-------------------------------------------------------------------------

(defsyscall* (%%sys-open "open") :int
  (pathname filename-designator)
  (flags    :int)
  (mode     mode-t))

(defvar *default-open-mode* #o666)

(defentrypoint %sys-open (pathname flags &optional (mode *default-open-mode*))
  (%%sys-open pathname flags mode))

(defsyscall* (%sys-creat "creat") :int
  (pathname filename-designator)
  (mode     mode-t))

(defsyscall (%%sys-pipe "pipe") :int
  (filedes :pointer))

(defentrypoint %sys-pipe ()
  "Create pipe, returns two values with the new FDs."
  (with-foreign-object (filedes :int 2)
    (%%sys-pipe filedes)
    (values (mem-aref filedes :int 0)
            (mem-aref filedes :int 1))))

(defsyscall (%sys-mkfifo "mkfifo") :int
  "Create a FIFO (named pipe)."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%sys-umask "umask") mode-t
  "Sets the umask and returns the old one"
  (new-mode mode-t))

(defsyscall (%sys-lseek (#+linux "lseek64" "lseek"))
    off-t
  (fildes :int)
  (offset off-t)
  (whence :int))

(defsyscall (%sys-access "access") :int
  (path  filename-designator)
  (amode :int))

(defsyscall* (%sys-truncate (#+linux "truncate64" "truncate"))
    :int
  (path   filename-designator)
  (length off-t))

(defsyscall* (%sys-ftruncate (#+linux "ftruncate64" "ftruncate"))
    :int
  (fd     :int)
  (length off-t))

(defsyscall (%sys-rename "rename") :int
  "Rename a file."
  (old filename-designator)
  (new filename-designator))

(defsyscall (%sys-link "link") :int
  (path1 filename-designator)
  (path2 filename-designator))

(defsyscall (%sys-symlink "symlink") :int
  "Creates a symbolic link"
  (name1 filename-designator)
  (name2 filename-designator))

(defsyscall (%%sys-readlink "readlink") ssize-t
  (path    filename-designator)
  (buf     :pointer)
  (bufsize size-t))

(defentrypoint %sys-readlink (path)
  "Read value of a symbolic link."
  (with-foreign-pointer (buf 4096 bufsize)
    (let ((count (%%sys-readlink path buf bufsize)))
      (values (foreign-string-to-lisp buf :count count)))))

(defsyscall (%sys-unlink "unlink") :int
  (path filename-designator))

(defsyscall* (%sys-chown "chown") :int
  "Change ownership of a file."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall* (%sys-fchown "fchown") :int
  "Change ownership of an open file."
  (fd    :int)
  (owner uid-t)
  (group uid-t))

(defsyscall* (%sys-lchown "lchown") :int
  "Change ownership of a file or symlink."
  (path  filename-designator)
  (owner uid-t)
  (group uid-t))

(defsyscall* (%sys-chmod "chmod") :int
  (path filename-designator)
  (mode mode-t))

(defsyscall* (%sys-fchmod "fchmod") :int
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
    :int
  #+linux
  (version   :int)
  (filedes :int)
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
  "Get information about a file."
  (funcall-stat #'%%sys-stat path))

(defentrypoint %sys-fstat (fd)
  "Get information about a file descriptor"
  (funcall-stat #'%%sys-fstat fd))

(defentrypoint %sys-lstat (path)
  "Get information about a file or symlink."
  (funcall-stat #'%%sys-lstat path))

(defsyscall (%sys-sync "sync") :void
  "Schedule all file system buffers to be written to disk.")

(defsyscall* (%sys-fsync "fsync") :int
  (fildes :int))

(defsyscall (%%sys-mkstemp "mkstemp") :int
  (template filename-designator))

(defentrypoint %sys-mkstemp (&optional (template ""))
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkstemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; Directories
;;;-------------------------------------------------------------------------

(defsyscall (%sys-mkdir "mkdir") :int
  "Create a directory."
  (path filename-designator)
  (mode mode-t))

(defsyscall (%sys-rmdir "rmdir") :int
  (path filename-designator))

(defsyscall (%sys-chdir "chdir") :int
  "Changes the current working directory"
  (path filename-designator))

(defsyscall* (%sys-fchdir "fchdir") :int
  (fildes :int))

(defsyscall (%%sys-getcwd "getcwd") :string
  (buf :pointer)
  (size size-t))

(defentrypoint %sys-getcwd ()
  "Returns the current working directory as a string."
  (with-foreign-pointer (buf path-max size)
    (%%sys-getcwd buf size)))

(defsyscall (%%sys-mkdtemp "mkdtemp") :int
  (template filename-designator))

(defentrypoint %sys-mkdtemp (&optional (template ""))
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-foreign-string (ptr (filename template))
      (values (%%sys-mkdtemp ptr) (foreign-string-to-lisp ptr)))))


;;;-------------------------------------------------------------------------
;;; File Descriptors
;;;-------------------------------------------------------------------------

(defsyscall (%sys-close "close") :int
  "Close an open file descriptor."
  (fd :int))

(defsyscall (%sys-dup "dup") :int
  (fildes :int))

(defsyscall* (%sys-dup2 "dup2") :int
  (fildes1 :int)
  (fildes2 :int))

(defsyscall* (%sys-ioctl/2 "ioctl") :int
  (fd      :int)
  (request :int))

(defsyscall* (%sys-ioctl/3 "ioctl") :int
 (fd      :int)
 (request :int)
 (arg     :pointer))

(defentrypoint %sys-fd-open-p (fd)
  (not (minusp (%sys-fstat fd))))


;;;-------------------------------------------------------------------------
;;; File descriptor polling
;;;-------------------------------------------------------------------------

;;; FIXME: Until a way to autodetect platform features is implemented
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'pollrdhup)
    (defconstant pollrdhup 0)))

(defsyscall (%sys-poll "poll") :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds-t)
  (timeout :int))


;;;-------------------------------------------------------------------------
;;; Directory walking
;;;-------------------------------------------------------------------------

(defsyscall (%sys-opendir "opendir") :pointer
  "Opens a directory for listing of its contents"
  (filename filename-designator))

(defsyscall (%sys-closedir "closedir") :int
  "Closes a directory when done listing its contents"
  (dir :pointer))

(defcfun* (%%sys-readdir-r (#+linux "readdir64_r" "readdir_r"))
    (return-wrapper :int :error-predicate (lambda (r) (not (zerop r)))
                    :error-generator signal-posix-error-from-return-value)
  (dirp   :pointer)
  (entry  :pointer)
  (result :pointer))

(defentrypoint %sys-readdir (dir)
  "Reads an item from the listing of a directory (reentrant)"
  (with-foreign-objects ((entry 'dirent) (result :pointer))
    (%%sys-readdir-r dir entry result)
    (if (null-pointer-p (mem-ref result :pointer))
        nil
        (with-foreign-slots ((name type fileno) entry dirent)
          (values (foreign-string-to-lisp name) type fileno)))))

(defsyscall (%sys-rewinddir "rewinddir") :void
  "Rewinds a directory."
  (dir :pointer))

(defsyscall (%sys-seekdir "seekdir") :void
  "Seeks a directory."
  (dir :pointer)
  (pos :long))

;;; FIXME: According to POSIX docs "no errors are defined" for
;;; telldir() but Linux manpages specify a possible EBADF.
(defsyscall (%sys-telldir "telldir") off-t
  "Returns the current location in a directory"
  (dir :pointer))


;;;-------------------------------------------------------------------------
;;; Memory mapping
;;;-------------------------------------------------------------------------

(defsyscall (%sys-mmap (#+linux "mmap64" "mmap"))
    :pointer
  (start  :pointer)
  (length size-t)
  (prot   :int)
  (flags  :int)
  (fd     :int)
  (offset off-t))

(defsyscall (%sys-munmap "munmap") :int
  "Unmap pages of memory."
  (addr :pointer)
  (len  size-t))


;;;-------------------------------------------------------------------------
;;; Process creation and info
;;;-------------------------------------------------------------------------

(defsyscall (%sys-fork "fork") pid-t
  "Create a child process.")

(defsyscall (%sys-getpid "getpid") pid-t
  "Returns the process id of the current process")

(defsyscall (%sys-getppid "getppid") pid-t
  "Returns the process id of the current process's parent")

(defsyscall (%sys-getuid "getuid") uid-t
  "Get real user id of the current process.")

(defsyscall (%sys-setuid "setuid") :int
  "Set real user id of the current process."
  (uid uid-t))

(defsyscall (%sys-geteuid "geteuid") uid-t
  "Get effective user id of the current process.")

(defsyscall (%sys-seteuid "seteuid") :int
  "Set effective user id of the current process."
  (uid uid-t))

(defsyscall (%sys-getgid "getgid") gid-t
  "Get real group id of the current process.")

(defsyscall (%sys-setgid "setgid") :int
  "Set real group id of the current process."
  (gid gid-t))

(defsyscall (%sys-getegid "getegid") gid-t
  "Get effective group id of the current process.")

(defsyscall (%sys-setegid "setegid") :int
  "Set effective group id of the current process."
  (gid gid-t))

(defsyscall (%sys-setreuid "setreuid") :int
  "Set real and effective user id of the current process."
  (ruid uid-t)
  (euid uid-t))

(defsyscall (%sys-setregid "setregid") :int
  "Set real and effective group id of the current process."
  (rgid gid-t)
  (egid gid-t))

(defsyscall (%sys-getpgid "getpgid") pid-t
  "Get process group id of a process."
  (pid pid-t))

(defsyscall (%sys-setpgid "setpgid") :int
  "Set process group id of a process."
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
  (with-foreign-object (rl 'rlimit)
    (with-foreign-slots ((cur max) rl rlimit)
      (%%sys-getrlimit resource rl)
      (values cur max))))

(defsyscall (%%sys-setrlimit (#+linux "setrlimit64" "setrlimit"))
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint %sys-setrlimit (resource soft-limit hard-limit)
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
  (which :int)
  (who   :int))

(defsyscall (%sys-setpriority "setpriority") :int
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

(defsyscall* (%sys-usleep "usleep") :int
  (useconds useconds-t))

(defsyscall (%%sys-time "time") time-t
  (tloc :pointer))

(defentrypoint %sys-time ()
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

  (defsyscall (%sys-mach-host-self "mach_host_self") port-t)

  (defsyscall (%%sys-host-get-clock-service "host_get_clock_service") kern-return-t
    (host port-t)
    (id clock-id-t)
    (clock-name (:pointer clock-serv-t)))

  (defentrypoint %sys-host-get-clock-service (id &optional (host (%sys-mach-host-self)))
    (with-foreign-object (clock 'clock-serv-t)
      (%%sys-host-get-clock-service host id clock)
      (mem-ref clock :int)))

  (defsyscall (%clock-get-time "clock_get_time") kern-return-t
    (clock-serv clock-serv-t)
    (cur-time timespec))

  (defentrypoint clock-get-time (clock-service)
    (with-foreign-object (time 'timespec)
      (%clock-get-time clock-service time)
      (with-foreign-slots ((tv-sec tv-nsec) time timespec)
        (values tv-sec tv-nsec)))))

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
  "Returns the value of an environment variable"
  (name :string))

(defsyscall (%sys-setenv "setenv") :int
  "Changes the value of an environment variable"
  (name      :string)
  (value     :string)
  (overwrite bool-designator))

(defsyscall (%sys-unsetenv "unsetenv") :int
  "Removes the binding of an environment variable"
  (name :string))


;;;-------------------------------------------------------------------------
;;; Hostname info
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-gethostname "gethostname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-gethostname ()
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%%sys-gethostname cstr size)))

(defsyscall (%%sys-getdomainname "getdomainname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint %sys-getdomainname ()
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

(defcfun (%%sys-getpwuid-r "getpwuid_r")
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
  (uid     uid-t)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defcfun (%%sys-getpwnam-r "getpwnam_r")
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
  "Gets the password-entry of a user, by user id. (reentrant)"
  (funcall-getpw #'%%sys-getpwuid-r uid))

(defentrypoint %sys-getpwnam (name)
  "Gets the password-entry of a user, by username. (reentrant)"
  (funcall-getpw #'%%sys-getpwnam-r name))


;;;-------------------------------------------------------------------------
;;; Group info
;;;-------------------------------------------------------------------------

(defsyscall (%%sys-getgrgid-r "getgrgid_r")
    (return-wrapper :int :error-predicate (lambda (x) (not (zerop x)))
                    :error-generator signal-posix-error-from-return-value)
  (uid     uid-t)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%%sys-getgrnam-r "getgrnam_r")
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
