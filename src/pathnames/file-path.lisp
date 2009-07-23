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

(defun split-name/type (file)
  (let* ((file (ustring-to-string* file))
         (dotpos (position #\. file :start 1 :from-end t)))
    (if (null dotpos)
        (values file nil)
        (values (subseq file 0 dotpos)
                (subseq file (1+ dotpos))))))

(defmethod file-path-name ((path file-path))
  (when-let ((file (slot-value path 'file)))
    (nth-value 0 (split-name/type file))))

(defmethod file-path-type ((path file-path))
  (when-let ((file (slot-value path 'file)))
    (nth-value 1 (split-name/type file))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun directory-name-p (name)
  (or (stringp name) (ustringp name)))

(defun relative-dir-p (dir)
  (and (listp dir) (not (eql :root (car dir)))))

(defun file-path-directory-p (directory)
  (and (consp directory)
       (or (eql :root (car directory))
           (directory-name-p (car directory)))
       (every #'directory-name-p (cdr directory))))

(defun split-root/dirs (dir)
  (if (eql :root (car dir))
      (values :root (cdr dir))
      (values nil   dir)))

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
    (dolist (dir (remove-if (lambda (c) (member c '(nil :root)))
                            (list* file directory)))
      (when (zerop (length dir)) (null-error))
      (when (delimp file) (slash-error file))))
  (setf (slot-value path 'file)
        (if (or (stringp file) (ustringp file))
            (ustring file)
            file))
  (setf (slot-value path 'directory)
        (if (consp directory)
            (multiple-value-bind (root dirs)
                (split-root/dirs directory)
              (if (eql :root root)
                  (cons :root (mapcar #'ustring dirs))
                  (mapcar #'ustring dirs)))
            directory)))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun file-path-p (thing)
  (typep thing 'file-path))

(defun file-path-absolute-p (path)
  (check-type path file-path)
  (not (relative-dir-p (file-path-directory path))))

(defun file-path-relative-p (path)
  (check-type path file-path)
  (relative-dir-p (file-path-directory path)))


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
                 :directory (or (when (and (relative-dir-p (file-path-directory path))
                                           (listp (file-path-directory defaults)))
                                  (append (file-path-directory defaults)
                                          (file-path-directory path)))
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
