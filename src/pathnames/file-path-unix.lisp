;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib.pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass unix-path (file-path)
  ()
  (:default-initargs :host :unspecific
                     :device :unspecific))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod initialize-instance :after ((path unix-path) &key)
  (with-slots (host device)
      path
    (check-type host (eql :unspecific))
    (check-type device (eql :unspecific))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun unix-path-p (thing)
  (typep thing 'unix-path))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defun enough-file-path (pathspec &optional
                         (defaults *default-file-path-defaults*))
  (let ((path (file-path pathspec))
        (defaults (file-path defaults)))
    (cond
      ((or (relative-file-path-p path)
           (relative-file-path-p defaults))
       path)
      (t
       (let* ((dir (cdr (slot-value path 'components)))
              (mismatch
               (mismatch dir (cdr (slot-value defaults 'components))
                         :test #'ustring=)))
         (if mismatch
             (make-instance 'unix-path :components (subseq dir mismatch))
             (make-instance 'unix-path :components (list :root))))))))

(defun %file-path-host-namestring (path)
  (declare (ignore path))
  "")

(defun %file-path-device-namestring (path)
  (declare (ignore path))
  "")

(defun %components-namestring (components print-dot trailing-delimiter)
  (multiple-value-bind (root dirs)
      (split-root/nodes components)
    (let ((delimstr (ustring +directory-delimiter+))
          (nullstr (ustring "")))
      (concatenate 'simple-ustring
                   (if (eql :root root)
                       delimstr
                       (if (and (null dirs) print-dot)
                           (ustring ".")
                           nullstr))
                   (apply #'join/ustring +directory-delimiter+ dirs)
                   (if (and dirs trailing-delimiter) delimstr nullstr)))))

(defun %file-path-components-namestring (path &key print-dot trailing-delimiter)
  (ustring-to-string*
   (%components-namestring (slot-value path 'components)
                           print-dot trailing-delimiter)))

(defun %file-path-directory-namestring (path)
  (if-let (dir (%file-path-directory path))
    (ustring-to-string*
     (%components-namestring dir t t))
    ""))

(defun %file-path-file-namestring (path)
  (if-let (file (%file-path-file path))
    (ustring-to-string* file)
    ""))

(defmethod file-path-namestring ((path unix-path))
  (with-slots (components trailing-delimiter)
      path
    (ustring-to-string*
     (%components-namestring components t trailing-delimiter))))

(defun file-path-namestring/ustring (path)
  (with-slots (components trailing-delimiter)
      path
    (%components-namestring components t trailing-delimiter)))

(defmethod file-path-namestring (pathspec)
  (file-path-namestring (file-path pathspec)))

(defun split-directory-namestring (namestring)
  (split-sequence-if (lambda (c) (uchar= c +directory-delimiter+))
                     namestring
                     :remove-empty-subseqs t))

(defun absolute-namestring-p (namestring)
  (uchar= +directory-delimiter+ (aref namestring 0)))

(defun parse-file-path (pathspec &key (start 0) end as-directory (expand-user t))
  (check-type pathspec (or string ustring))
  (when (zerop (length pathspec))
    (error 'invalid-file-path
           :path pathspec
           :reason "Paths of null length are not valid"))
  (let* ((actual-namestring (subseq (ustring pathspec) start end))
         (expansion (or (when expand-user
                          (prog1
                              (ignore-some-conditions (isys:syscall-error)
                                (%expand-user-directory actual-namestring))
                            (when (ustring= pathspec "~")
                              (setf as-directory t))))
                        actual-namestring))
         (components (split-directory-namestring expansion))
         (trailing-delimiter-p (or as-directory
                                   (uchar= +directory-delimiter+
                                           (aref actual-namestring
                                                 (1- (length actual-namestring)))))))
    (make-instance 'unix-path
                   :components (if (absolute-namestring-p expansion)
                                   (cons :root components)
                                   components)
                   :trailing-delimiter trailing-delimiter-p)))

(defun %expand-user-directory (pathspec)
  (flet ((user-homedir (user)
           ;; TODO: use our own babel ustring<->UTF8 codec instead of u-t-s*
           (nth-value 5 (isys:%sys-getpwnam (ustring-to-string* user))))
         (uid-homedir (uid)
           (nth-value 5 (isys:%sys-getpwuid (ustring-to-string* uid)))))
    (unless (uchar= (char-to-uchar #\~) (aref pathspec 0))
      (return* pathspec))
    (destructuring-bind (first &rest rest)
        (split-directory-namestring pathspec)
      (let ((homedir
             (cond
               ((ustring= "~" first)
                (or (isys:%sys-getenv "HOME")
                    (let ((username
                           (or (isys:%sys-getenv "USER")
                               (isys:%sys-getenv "LOGIN"))))
                      (if username
                          (user-homedir username)
                          (uid-homedir (isys:%sys-getuid))))))
               ((uchar= (char-to-uchar #\~) (aref first 0))
                (user-homedir (subseq first 1)))
               (t
                (bug "The pathspec is suppose to start with a ~S" #\~)))))
        (if homedir
            (apply #'join/ustring +directory-delimiter+ (ustring homedir) rest)
            pathspec)))))


;;;-------------------------------------------------------------------------
;;; Specials
;;;-------------------------------------------------------------------------

(defparameter *default-file-path-defaults*
  (or (ignore-some-conditions (isys:syscall-error)
        (parse-file-path (isys:%sys-getcwd) :as-directory t))
      (ignore-some-conditions (isys:syscall-error)
        (parse-file-path "~"))
      (parse-file-path "/")))

(defparameter *default-execution-path*
  (mapcar (lambda (p)
            (parse-file-path p :as-directory t))
          (split-sequence +execution-path-delimiter+ (ustring (isys:%sys-getenv "PATH"))
                          :remove-empty-subseqs t)))
