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
   (file :initarg :file
         :initform nil)
   (trailing-delimiter :initarg :trailing-delimiter
                       :initform nil
                       :reader file-path-trailing-delimiter)))

(define-condition invalid-file-path (isys:iolib-error)
  ((path :initarg :path :reader invalid-file-path-path)
   (reason :initform nil :initarg :reason :reader invalid-file-path-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid file path: ~S."
                     (ustring-to-string*
                      (ustring
                       (invalid-file-path-path condition))))
             (when-let (reason (invalid-file-path-reason condition))
               (format stream "~%~A." reason)))))


;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +file-path-host-type+
    #+unix    'unix-path
    #+windows 'unc-path))

(defconstant +directory-delimiter+
  #+unix    (uchar #\/)
  #+windows (uchar #\\))

(defconstant +alternative-delimiter+
  #+unix    nil
  #+windows (uchar #\/))

(defconstant (+directory-delimiters+ :test #'equal)
  (list* +directory-delimiter+ +alternative-delimiter+))

(defconstant +execution-path-delimiter+
  #+unix    (uchar #\:)
  #+windows (uchar #\;))

(declaim (special *default-file-path-defaults*))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

;;; Accessors

(defgeneric file-path-host (path &key namestring))

(defgeneric file-path-device (path &key namestring))

(defgeneric file-path-directory (path &key namestring))

(defgeneric file-path-file (path &key namestring))

(defgeneric file-path-type (path))

(defgeneric file-path-namestring (path))

;;; Operations

(defgeneric make-file-path (&key host device directory file defaults))

(defgeneric merge-file-paths (path &optional defaults))

(defgeneric enough-file-path (path &optional defaults))

(defgeneric file-path (pathspec))

(defgeneric parse-file-path (pathspec &key start end as-directory expand-user))

(defgeneric expand-user-directory (path))

;;; Internal functions

(defgeneric %file-path-directory-namestring (path &key print-dot))

(defgeneric %expand-user-directory (pathspec))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod file-path-host ((path file-path) &key namestring)
  (if namestring
      (%file-path-host-namestring path)
      (slot-value path 'host)))

(defmethod file-path-device ((path file-path) &key namestring)
  (if namestring
      (%file-path-device-namestring path)
      (slot-value path 'device)))

(defmethod file-path-directory ((path file-path) &key namestring)
  (if namestring
      (%file-path-directory-namestring path :print-dot t
                                       :trailing-delimiter t)
      (slot-value path 'directory)))

(defmethod file-path-file ((path file-path) &key namestring)
  (if namestring
      (%file-path-file-namestring path)
      (slot-value path 'file)))

(defmethod file-path-type ((path file-path))
  (when-let* ((file (slot-value path 'file))
              (type (cadr (split-sequence (char-to-uchar #\.) file
                                          :from-end t :count 2))))
    (ustring-to-string* type)))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun directory-name-p (name)
  (or (stringp name) (ustringp name)))

(defun file-path-directory-p (directory)
  (and (consp directory)
       (member (car directory) '(:absolute :relative))
       (every #'directory-name-p (cdr directory))))

(defmethod initialize-instance :after ((path file-path) &key directory file)
  (check-type directory (or null (eql :unspecific)
                            (satisfies file-path-directory-p)))
  (check-type file (or null (eql :unspecific) string ustring))
  (flet ((null-error ()
           (error 'invalid-file-path :path ""
                  :reason "Null filenames are not valid"))
         (slash-error (path)
           (error 'invalid-file-path :path (ustring-to-string* (ustring path))
                  :reason "Filenames cannot contain directory delimiters(#\\ and #\/)"))
         (delimp (path)
           (find-if (lambda (c) (member c +directory-delimiters+)) (ustring path))))
    (dolist (dir (list* file (cdr directory)))
      (when dir
        (when (zerop (length dir)) (null-error))
        (when (delimp file) (slash-error file)))))
  (setf (slot-value path 'file)
        (if (or (stringp file) (ustringp file))
            (ustring file)
            file))
  (setf (slot-value path 'directory)
        (if (consp directory)
            (list* (car directory) (mapcar #'ustring (cdr directory)))
            directory)))


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

(defmethod file-path ((pathspec file-path))
  pathspec)

(defmethod file-path (pathspec)
  (parse-file-path pathspec))

(defmethod make-file-path (&key (host nil hostp) (device nil devicep)
                           (directory nil directoryp) (file nil filep)
                           defaults)
  (check-type defaults (or null file-path))
  (make-instance '#.+file-path-host-type+
                 :host (cond (hostp    host)
                             (defaults (file-path-host defaults))
                             (t        (file-path-host
                                        *default-file-path-defaults*)))
                 :device (cond (devicep  device)
                               (defaults (file-path-device defaults))
                               (t        (file-path-device
                                          *default-file-path-defaults*)))
                 :directory (cond (directoryp directory)
                                  (defaults   (file-path-directory defaults))
                                  (t          (file-path-directory
                                               *default-file-path-defaults*)))
                 :file (cond (filep    file)
                             (defaults (file-path-file defaults))
                             (t        (file-path-file
                                        *default-file-path-defaults*)))))

(defmethod merge-file-paths ((path file-path) &optional
                             (defaults *default-file-path-defaults*))
  (check-type defaults file-path)
  (make-instance '#.+file-path-host-type+
                 :host (or (file-path-host path)
                           (file-path-host defaults))
                 :device (or (file-path-device path)
                             (file-path-device defaults))
                 :directory (or (let ((dir (file-path-directory path)))
                                  (when (and dir (eql :relative (car dir))
                                             (listp (file-path-directory defaults)))
                                    (append (file-path-directory defaults) (cdr dir))))
                                (file-path-directory defaults))
                 :name (or (file-path-file path)
                           (file-path-file defaults))))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((path file-path) stream)
  (format stream "#/p/~S" (file-path-namestring path)))

(define-literal-reader p (stream)
  (let ((token (read stream)))
    (check-type token string)
    (file-path token)))
