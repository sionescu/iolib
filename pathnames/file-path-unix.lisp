;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib.pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass unix-path (file-path)
  ((directory-delimiter
    :initform #\/)
   (alternative-delimiter
    :initform nil)
   (execution-path-delimiter
    :initform #\:))
  (:default-initargs :host :unspecific
                     :device :unspecific))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun unix-path-p (thing)
  (typep thing 'unix-path))

(defun absolute-namestring-p (namestring)
  (not (null (ppcre:scan +absolute-directory-regex+ namestring))))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defmethod enough-file-path ((path unix-path) &optional
                             (defaults *default-file-path-defaults*))
  (cond
    ((or (file-path-relative-p path)
         (file-path-relative-p defaults))
     path)
    (t
     (multiple-value-bind (dirtype enough-directory)
         (if (equal (cadr (file-path-directory path))
                    (cadr (file-path-directory defaults)))
             (values :relative
                     (loop :for rest1 :on (cddr (file-path-directory path))
                           :for rest2 :on (cddr (file-path-directory defaults))
                           :if (not (equal (car rest1) (car rest2))) :do (loop-finish)
                           :finally (return rest1)))
             (values :absolute (cdr (file-path-directory path))))
       (make-instance 'unix-path :directory (list* dirtype enough-directory)
                      :name (file-path-name path))))))

(defmethod file-path-namestring ((path unix-path))
  (with-slots (directory name)
      path
    (with-output-to-string (stream)
      (princ (%file-path-directory-namestring path :trailing-delimiter t)
             stream)
      (when (stringp name)
        (princ name stream)))))

(defmethod %file-path-directory-namestring ((path unix-path) &key trailing-delimiter)
  (with-slots (directory)
      path
    (with-output-to-string (stream)
      (when (consp directory)
        (destructuring-bind (directory-type &rest dirs)
            directory
          (ecase directory-type
            (nil t)   ; no directory
            (:absolute
             (princ (file-path-directory-delimiter path) stream))
            (:relative t))
          (princ (apply #'join (file-path-directory-delimiter path)
                        (if trailing-delimiter (append dirs (list "")) dirs))
                 stream))))))

(defun split-directory-namestring (namestring &optional limit)
  (remove "" (ppcre:split +split-directories-regex+ namestring
                          :limit limit)
          :test #'string=))

(defmethod parse-file-path-type ((namestring string)
                                 (type (eql 'unix-path))
                                 &key as-directory expand-user)
  (let* ((expansion (if expand-user
                        (expand-userdir namestring)
                        namestring))
         (components (remove "." (split-directory-namestring expansion)
                             :test #'string=))
         (dirname (if as-directory components (butlast components)))
         (basename (if as-directory nil (lastcar components)))
         (directory-type (if (absolute-namestring-p expansion)
                             :absolute
                             :relative)))
    (make-instance type :directory (cons directory-type dirname)
                   :name (if (string= "" basename) nil basename))))

(defmethod expand-userdir ((dirname string))
  (flet ((user-homedir (user)
           (nth-value 5 (nix:getpwnam user)))
         (uid-homedir (uid)
           (nth-value 5 (nix:getpwuid uid)))
         (concat-homedir (dir rest)
           (join +directory-delimiter+ dir rest)))
    (destructuring-bind (first &optional rest)
        (split-directory-namestring dirname 2)
      (cond
        ((string= "~" first)
         (let ((homedir
                (or (isys:%sys-getenv "HOME")
                    (let ((username
                           (or (isys:%sys-getenv "USER")
                               (isys:%sys-getenv "LOGIN"))))
                      (and username
                           (user-homedir username)
                           (uid-homedir (nix:getuid)))))))
           (return* (concat-homedir homedir rest))))
        ((char= #\~ (char first 0))
         (let ((homedir
                (and (user-homedir (subseq first 1))
                     first)))
           (return* (concat-homedir homedir rest))))
        (t
         dirname)))))


;;;-------------------------------------------------------------------------
;;; Specials
;;;-------------------------------------------------------------------------

(defparameter *default-file-path-defaults*
  (parse-file-path (isys:%sys-getcwd) :as-directory t))

(defparameter *default-execution-path*
  (mapcar (lambda (p)
            (parse-file-path p :as-directory t))
          (split-sequence +execution-path-delimiter+ (isys:%sys-getenv "PATH")
                          :remove-empty-subseqs t)))
