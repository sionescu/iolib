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
   (components :initarg :components
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

(defgeneric file-path-components (path &key namestring))

(defgeneric file-path-directory (path &key namestring))

(defgeneric file-path-file (path &key namestring))

(defgeneric file-path-file-name (path))

(defgeneric file-path-file-type (path))

(defgeneric file-path-namestring (path))

;;; Operations

(defgeneric make-file-path (&key host device components defaults))

(defgeneric merge-file-paths (path &optional defaults))

(defgeneric enough-file-path (path &optional defaults))

(defgeneric file-path (pathspec))

(defgeneric parse-file-path (pathspec &key start end as-directory expand-user))

(defgeneric expand-user-directory (path))

;;; Internal functions

(defgeneric %file-path-host-namestring (path))

(defgeneric %file-path-device-namestring (path))

(defgeneric %file-path-components-namestring
    (path &key print-dot trailing-delimiter))

(defgeneric %file-path-directory-namestring (path))

(defgeneric %file-path-file-namestring (path))

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

(defmethod file-path-components ((path file-path) &key namestring)
  (if namestring
      (%file-path-components-namestring
       path
       :print-dot t
       :trailing-delimiter (file-path-trailing-delimiter path))
      (slot-value path 'components)))

(defun split-root/nodes (dir)
  (if (eql :root (car dir))
      (values :root (cdr dir))
      (values nil   dir)))

(defun %file-path-directory (path)
  (let ((components (slot-value path 'components)))
    (multiple-value-bind (root nodes)
        (split-root/nodes components)
      (cons root (butlast nodes)))))

(defmethod file-path-directory ((path file-path) &key namestring)
  (if namestring
      (%file-path-directory-namestring path)
      (%file-path-directory path)))

(defun %file-path-file (path)
  (let ((components (slot-value path 'components)))
    (lastcar (nth-value 1 (split-root/nodes components)))))

(defmethod file-path-file ((path file-path) &key namestring)
  (if namestring
      (%file-path-file-namestring path)
      (%file-path-file path)))

(defun split-name/type (file)
  (let* ((file (ustring-to-string* file))
         (dotpos (position #\. file :start 1 :from-end t)))
    (if (null dotpos)
        (values file nil)
        (values (subseq file 0 dotpos)
                (subseq file (1+ dotpos))))))

(defmethod file-path-name ((path file-path))
  (when-let (file (%file-path-file path))
    (nth-value 0 (split-name/type file))))

(defmethod file-path-type ((path file-path))
  (when-let (file (%file-path-file path))
    (nth-value 1 (split-name/type file))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun valid-component-types-p (components)
  (multiple-value-bind (root nodes)
      (split-root/nodes components)
    (and (member root '(nil :root))
         (every #'(lambda (n)
                    (or (stringp n) (ustringp n)))
                nodes))))

(defmethod initialize-instance :after ((path file-path) &key components)
  (check-type components (and (not null) (satisfies valid-component-types-p)))
  (setf (slot-value path 'components)
        (mapcar (lambda (n) (if (eql :root n) :root (ustring n)))
                components))
  (dolist (node (cdr (slot-value path 'components)))
    (when (zerop (length node))
      (error 'invalid-file-path :path ""
             :reason "Null filenames are not valid"))
    (when (find-if (lambda (c) (member c +directory-delimiters+)) node)
      (error 'invalid-file-path :path node
             :reason "Path components cannot contain directory delimiters(#\\ and #\/)"))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun file-path-p (thing)
  (typep thing 'file-path))

(defun absolute-p (dir)
  (eql :root (car dir)))

(defun absolute-file-path-p (path)
  (check-type path file-path)
  (absolute-p (slot-value path 'components)))

(defun relative-file-path-p (path)
  (check-type path file-path)
  (not (absolute-p (slot-value path 'components))))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defmethod file-path ((pathspec file-path))
  pathspec)

(defmethod file-path (pathspec)
  (parse-file-path pathspec))

(defmethod file-path ((pathspec pathname))
  (parse-file-path (namestring pathspec)))

(defmethod make-file-path (&key (host nil hostp) (device nil devicep)
                           (components nil componentsp) defaults)
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
                 :components (cond (componentsp components)
                                   (defaults   (file-path-components defaults))
                                   (t          (file-path-components
                                                *default-file-path-defaults*)))))

(defmethod merge-file-paths ((path file-path) &optional
                             (defaults *default-file-path-defaults*))
  (check-type defaults file-path)
  (make-instance '#.+file-path-host-type+
                 :host (or (file-path-host path)
                           (file-path-host defaults))
                 :device (or (file-path-device path)
                             (file-path-device defaults))
                 :components (if (absolute-p (file-path-components path))
                                 (append (file-path-components defaults)
                                         (file-path-components path))
                                 (file-path-components defaults))))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((path file-path) stream)
  (let ((ns (file-path-namestring path)))
    (if *print-escape*
        (format stream "#/p/~S" ns)
        (write-string ns stream))))

(define-literal-reader p (stream)
  (let ((token (read stream)))
    (check-type token string)
    (file-path token)))
