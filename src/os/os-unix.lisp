;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- OS interface.
;;;

(in-package :iolib.os)

;;;; Environment access

(defclass environment ()
  ((variables :initarg :variables
              :initform (make-hash-table :test #'equal)
              :accessor environment-variables)))

(defun %envar (env name)
  (gethash name (environment-variables env)))

(defun (setf %envar) (value env name)
  (setf (gethash name (environment-variables env))
        value))

(defun %remvar (env name)
  (remhash name (environment-variables env)))

(defun environment-variable (name &key env)
  "ENVIRONMENT-VARIABLE returns the environment variable
identified by NAME, or NIL if one does not exist.  NAME can
either be a symbol or a string.

SETF ENVIRONMENT-VARIABLE sets the environment variable
identified by NAME to VALUE.  Both NAME and VALUE can be either a
symbols or strings. Signals an error on failure."
  (let ((name (string name)))
    (cond
      (env
       (check-type env environment)
       (%envar env name))
      (t
       (%sys-getenv name)))))

(defun (setf environment-variable) (value name &key env (overwrite t))
  (check-type value string)
  (let ((name (string name)))
    (cond
      (env
       (check-type env environment)
       (when (or overwrite
                 (null (nth-value 1 (%envar env name))))
         (setf (%envar env name) value)))
      (t
       (%sys-setenv (string name) value overwrite))))
  value)

(defun makunbound-environment-variable (name &key env)
  "Removes the environment variable identified by NAME from the
current environment.  NAME can be either a string or a symbol.
Returns the string designated by NAME.  Signals an error on
failure."
  (let ((name (string name)))
    (cond
      (env
       (check-type env environment)
       (%remvar env name))
      (t
       (%sys-unsetenv (string name)))))
  (values name))

(defun %environment ()
  (loop :with env := (make-instance 'environment)
        :for i :from 0 :by 1
        :for string := (mem-aref *environ* :string i)
        :for split := (position #\= string)
        :while string :do
        (let ((var (subseq string 0 split))
              (val (subseq string (1+ split))))
          (setf (environment-variable var :env env) val))
        :finally (return env)))

(defun environment (&optional env)
  "If ENV is non-NIL, ENVIRONMENT copies ENV, otherwise returns the
current global environment.
SETF ENVIRONMENT replaces the contents of the global environment
with that of its argument.

Often it is preferable to use SETF ENVIRONMENT-VARIABLE and
MAKUNBOUND-ENVIRONMENT-VARIABLE to modify the environment instead
of SETF ENVIRONMENT."
  (cond
    (env
     (check-type env environment)
     (make-instance 'environment
                    :variables (copy-hash-table
                                (environment-variables env))))
    (t
     (%environment))))

(defun (setf environment) (newenv)
  (check-type newenv environment)
  (let ((oldenv (environment)))
    (maphash (lambda (k v)
               (setf (environment-variable k) v)
               (makunbound-environment-variable k :env oldenv))
             (environment-variables newenv))
    (maphash (lambda (k v) (declare (ignore v))
               (makunbound-environment-variable k))
             (environment-variables oldenv)))
  newenv)


;;;; Current directory

(defun current-directory ()
  "CURRENT-DIRECTORY returns the operating system's current
directory, which may or may not correspond to
*DEFAULT-FILE-PATH-DEFAULTS*.

SETF CURRENT-DIRECTORY changes the operating system's current
directory to the PATHSPEC.  An error is signalled if PATHSPEC
is not a directory."
  (let ((cwd (%sys-getcwd)))
    (if cwd
        (parse-file-path cwd :expand-user nil)
        (syscall-error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  (let ((path (file-path pathspec)))
    (%sys-chdir (file-path-namestring path))))

(defmacro with-current-directory (pathspec &body body)
  (with-gensyms (old)
    `(let ((,old (current-directory)))
       (unwind-protect
            (progn
              (setf (current-directory) (file-path ,pathspec))
              ,@body)
         (setf (current-directory) ,old)))))


;;;; File-path manipulations

(defun absolute-file-path (pathspec defaults)
  (let ((path (file-path pathspec)))
    (if (absolute-file-path-p path)
        path
        (let ((tmp (merge-file-paths path defaults)))
          (if (absolute-file-path-p tmp)
              tmp
              (merge-file-paths tmp (current-directory)))))))

(defun strip-dots (path)
  (multiple-value-bind (root nodes)
      (split-root/nodes (file-path-components path))
    (let (new-components)
      (dolist (n nodes)
        (cond
          ((string= n "."))
          ((string= n "..")
           (pop new-components))
          (t (push n new-components))))
      (make-file-path :components (if root
                                      (cons root (nreverse new-components))
                                      (nreverse new-components))
                      :defaults path))))

(defun resolve-symlinks (path)
  (let* ((namestring (file-path-namestring path))
         (realpath (%sys-realpath namestring)))
    (parse-file-path realpath)))

(defun resolve-file-path (pathspec &key
                          (defaults *default-file-path-defaults*)
                          (canonicalize t))
  "Returns an absolute file-path corresponding to PATHSPEC by
merging it with DEFAULT, and (CURRENT-DIRECTORY) if necessary.
If CANONICALIZE is non-NIL, the path is canonicalised: if it is :STRIP-DOTS,
then just remove «.» and «..», otherwise symlinks are resolved too."
  (let ((absolute-file-path (absolute-file-path pathspec defaults)))
    (case canonicalize
      ((nil)       absolute-file-path)
      (:strip-dots (strip-dots absolute-file-path))
      (t           (resolve-symlinks absolute-file-path)))))


;;;; File kind

;;; FIXME: make sure that GET-FILE-KIND be able to signal
;;;        only conditions of type FILE-ERROR, either by
;;;        wrapping POSIX-ERRORs or making sure that some
;;;        POSIX-ERRORS subclass FILE-ERROR
(defun get-file-kind (file follow-p)
  (let ((namestring (file-path-namestring file)))
    (handler-case
        (let ((mode (stat-mode
                     (if follow-p
                         (%sys-stat namestring)
                         (%sys-lstat namestring)))))
          (switch ((logand s-ifmt mode) :test #'=)
            (s-ifdir  :directory)
            (s-ifchr  :character-device)
            (s-ifblk  :block-device)
            (s-ifreg  :regular-file)
            (s-iflnk  :symbolic-link)
            (s-ifsock :socket)
            (s-ififo  :pipe)
            (t (bug "Unknown file mode: ~A." mode))))
      (enoent ()
        (cond
          ;; stat() returned ENOENT: either FILE does not exist
          ;; or it is a broken symlink
          (follow-p
           (handler-case
               (%sys-lstat namestring)
             (enoent ())
             (:no-error (stat)
               (declare (ignore stat))
               (values :symbolic-link :broken))))
          ;; lstat() returned ENOENT: FILE does not exist
          (t nil))))))

(defun file-kind (pathspec &key follow-symlinks)
  "Returns a keyword indicating the kind of file designated by PATHSPEC,
or NIL if the file does not exist.  Does not follow symbolic
links by default.

Possible file-kinds in addition to NIL are: :REGULAR-FILE,
:SYMBOLIC-LINK, :DIRECTORY, :PIPE, :SOCKET, :CHARACTER-DEVICE, and
:BLOCK-DEVICE.
If FOLLOW-SYMLINKS is non-NIL and PATHSPEC designates a broken symlink
returns :BROKEN as second value."
  (get-file-kind (merge-file-paths pathspec) follow-symlinks))

(defun file-exists-p (pathspec &optional file-kind)
  "Checks whether the file named by the file-path designator
PATHSPEC exists, if this is the case and FILE-KIND is specified
it also checks the file kind. If the tests succeed, return two values:
truename and file kind of PATHSPEC, NIL otherwise.
Follows symbolic links."
  (let* ((path (file-path pathspec))
         (follow (unless (eq file-kind :symbolic-link) t))
         (actual-kind (file-kind path :follow-symlinks follow)))
    (when (and actual-kind
               (if file-kind (eql file-kind actual-kind) t))
      (values (resolve-file-path path)
              actual-kind))))

(defun regular-file-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a regular file. Returns its truename
if this is the case, NIL otherwise. Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :regular-file)))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a directory.  Returns its truename
if this is the case, NIL otherwise.  Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :directory)))

(defun good-symlink-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a symlink pointing to an existent file."
  (eq :broken (nth-value 1 (file-kind pathspec :follow-symlinks t))))


;;;; Temporary files

(defvar *temporary-directory*
  (let ((system-tmpdir (or (environment-variable "TMPDIR")
                           (environment-variable "TMP")
                           "/tmp")))
    (parse-file-path system-tmpdir :expand-user nil)))


;;;; Symbolic and hard links

(defun read-link (pathspec)
  "Returns the file-path pointed to by the symbolic link
designated by PATHSPEC.  If the link is relative, then the
returned file-path is relative to the link, not
*DEFAULT-FILE-PATH-DEFAULTS*.

Signals an error if PATHSPEC is not a symbolic link."
  ;; Note: the previous version tried much harder to provide a buffer
  ;; big enough to fit the link's name.  OTOH, NIX:READLINK stack
  ;; allocates on most lisps.
  (file-path (%sys-readlink
              (file-path-namestring
               (absolute-file-path pathspec *default-file-path-defaults*)))))

(defun make-link (link target &key hard)
  "Creates LINK that points to TARGET.  Defaults to a symbolic
link, but giving a non-NIL value to the keyword argument :HARD
creates a hard link.  Returns the file-path of the link.

Relative targets are resolved against the link.  Relative links
are resolved against *DEFAULT-FILE-PATH-DEFAULTS*.

Signals an error if target does not exist, or link exists already."
  (let ((link (file-path link))
        (target (file-path target)))
    (with-current-directory (absolute-file-path *default-file-path-defaults* nil)
      ;; KLUDGE: We merge against link for hard links only,
      ;; since symlink does the right thing once we are in
      ;; the correct directory.
      (if hard
          (%sys-link (file-path-namestring
                      (merge-file-paths target link))
                     link)
          (%sys-symlink (file-path-namestring target)
                        (file-path-namestring link)))
      link)))


;;;; File permissions

(defconstant (+permissions+ :test #'equal)
  `((:user-read    . ,s-irusr)
    (:user-write   . ,s-iwusr)
    (:user-exec    . ,s-ixusr)
    (:group-read   . ,s-irgrp)
    (:group-write  . ,s-iwgrp)
    (:group-exec   . ,s-ixgrp)
    (:other-read   . ,s-iroth)
    (:other-write  . ,s-iwoth)
    (:other-exec   . ,s-ixoth)
    (:set-user-id  . ,s-isuid)
    (:set-group-id . ,s-isgid)
    (:sticky       . ,s-isvtx)))

(defun file-permissions (pathspec)
  "FILE-PERMISSIONS returns a list of keywords identifying the
permissions of PATHSPEC.

SETF FILE-PERMISSIONS sets the permissions of PATHSPEC as
identified by the symbols in list.

If PATHSPEC designates a symbolic link, that link is implicitly
resolved.

Permission symbols consist of :USER-READ, :USER-WRITE, :USER-EXEC,
:GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ, :OTHER-WRITE,
:OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY.

Both signal an error if PATHSPEC doesn't designate an existing file."
  (let ((mode (stat-mode (%sys-stat (file-path-namestring pathspec)))))
    (loop :for (name . value) :in +permissions+
          :when (plusp (logand mode value))
          :collect name)))

(defun (setf file-permissions) (perms pathspec)
  (%sys-chmod (file-path-namestring pathspec)
              (reduce (lambda (a b)
                        (logior a (cdr (assoc b +permissions+))))
                      perms :initial-value 0)))


;;;; User information

(defun user-info (id)
  "USER-INFO returns the password entry for the given name or
numerical user ID, as an assoc-list."
  (multiple-value-bind (name password uid gid gecos home shell)
      (etypecase id
        (string  (%sys-getpwnam id))
        (integer (%sys-getpwuid id)))
    (declare (ignore password))
    (unless (null name)
      (list (cons :name name)
            (cons :user-id uid)
            (cons :group-id gid)
            (cons :gecos gecos)
            (cons :home home)
            (cons :shell shell)))))
