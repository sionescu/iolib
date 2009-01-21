;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib.pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass file-path ()
  ((host :initarg :host)
   (device :initarg :device)
   (directory :initarg :directory
              :initform nil)
   (name :initarg :name
         :initform nil)
   (directory-delimiter
    :reader file-path-directory-delimiter
    :allocation :class)
   (alternative-delimiter
    :reader file-path-alternative-delimiter
    :allocation :class)
   (execution-path-delimiter
    :reader file-path-execution-path-delimiter
    :allocation :class)))


;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +file-path-host-type+
    #+unix 'unix-path #+windows 'unc-path))

(defconstant +directory-delimiter+
  #+unix #\/ #+windows #\\)

(defconstant +alternative-delimiter+
  #+unix nil #+windows #\/)

(defconstant (+split-directories-regex+ :test 'equal)
  (if +alternative-delimiter+
      (format nil "(~C|~C)" +directory-delimiter+ +alternative-delimiter+)
      +directory-delimiter+))

(defconstant (+absolute-directory-regex+ :test 'equal)
  (format nil "^~C" +split-directories-regex+))

(defconstant +execution-path-delimiter+
  #+unix #\: #+windows #\;)

(declaim (special *default-file-path-defaults*))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

;;; Accessors

(defgeneric file-path-host (path))

(defgeneric file-path-device (path))

(defgeneric file-path-directory (path &key namestring))

(defgeneric file-path-name (path))

(defgeneric file-path-namestring (path))

;;; Operations

(defgeneric make-file-path (&key host device directory name defaults type))

(defgeneric merge-file-paths (path &optional defaults))

(defgeneric enough-file-path (path &optional defaults))

(defgeneric concatenate-paths (&rest paths))

(defgeneric parse-file-path-type (namestring type &key start end
                                  as-directory expand-user))

(defgeneric parse-file-path (namestring &key start end
                             as-directory expand-user))

(defgeneric file-path (pathspec))

(defgeneric expand-user-directory (path))

;;; Internal functions

(defgeneric %file-path-directory-namestring (path &key trailing-delimiter))

(defgeneric %expand-user-directory (pathspec))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod file-path-host ((path file-path))
  (slot-value path 'host))

(defmethod file-path-device ((path file-path))
  (slot-value path 'device))

(defmethod file-path-directory ((path file-path) &key namestring)
  (if namestring
      (%file-path-directory-namestring path)
      (slot-value path 'directory)))

(defmethod file-path-name ((path file-path))
  (slot-value path 'name))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun directory-name-p (name)
  (and (stringp name) (not (ppcre:scan +split-directories-regex+ name))))

(defun file-path-directory-p (directory)
  (and (consp directory)
       (member (car directory) '(:absolute :relative))
       (every #'directory-name-p (cdr directory))))

(defmethod initialize-instance :after ((path file-path) &key directory name)
  (check-type directory (or null (eql :unspecific)
                            (satisfies file-path-directory-p)))
  (check-type name (or null (eql :unspecific) string)))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun file-path-p (thing)
  (typep thing 'file-path))

(defun file-path-absolute-p (thing)
  (and (file-path-p thing)
       (eql :absolute (car (file-path-directory thing)))))

(defun file-path-relative-p (thing)
  (and (file-path-p thing)
       (eql :relative (car (file-path-directory thing)))))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defmethod make-file-path (&key (type '#.+file-path-host-type+)
                           host device directory name defaults)
  (check-type defaults (or null file-path))
  (make-instance type
                 :host (or host
                           (if defaults
                               (file-path-host defaults)
                               (file-path-host *default-file-path-defaults*)))
                 :device (or device
                             (and defaults (file-path-device defaults)))
                 :directory (or directory
                                (and defaults (file-path-directory defaults)))
                 :name (or name
                           (and defaults (file-path-name defaults)))))

(defmethod merge-file-paths ((path file-path) &optional
                             (defaults *default-file-path-defaults*))
  (check-type defaults file-path)
  (make-instance (class-of path)
                 :host (or (file-path-host path)
                           (file-path-host defaults))
                 :device (or (file-path-device path)
                             (file-path-device defaults))
                 :directory (or (let ((dir (file-path-directory path)))
                                  (when (and dir (eql :relative (car dir))
                                             (listp (file-path-directory defaults)))
                                    (append dir (file-path-directory defaults))))
                                (file-path-directory defaults))
                 :name (or (file-path-name path)
                           (file-path-name defaults))))

(defmethod concatenate-paths (&rest paths)
  (assert (every #'file-path-p paths))
  (when (null paths) (return* nil))
  (let ((as-directory
         (not (stringp (file-path-name (lastcar paths)))))
        (big-namestring
         (apply #'join (file-path-directory-delimiter (car paths))
                (mapcar #'file-path-namestring paths))))
    (parse-file-path-type big-namestring (type-of (car paths))
                          :as-directory as-directory)))

(defun split-directory-namestring (namestring &optional limit)
  (remove "" (ppcre:split +split-directories-regex+ namestring
                          :limit limit)
          :test #'string=))

(defmethod parse-file-path (namestring &key (start 0) end
                            as-directory expand-user)
  (parse-file-path-type namestring +file-path-host-type+
                        :start start :end end
                        :as-directory as-directory
                        :expand-user expand-user))

(defmethod file-path ((pathspec file-path))
  pathspec)

(defmethod file-path ((pathspec string))
  (parse-file-path pathspec))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((path file-path) stream)
  (print-unreadable-object (path stream :type t)
    (format stream "~S" (file-path-namestring path))))
