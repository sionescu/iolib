;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib.pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +file-path-host-type+
    #+unix    'unix-path
    #+windows 'unc-path))

(defclass file-path ()
  ((host :initarg :host)
   (device :initarg :device)
   (components :initarg :components
               :initform nil)
   (trailing-delimiter :initarg :trailing-delimiter
                       :initform nil
                       :reader file-path-trailing-delimiter)))

(deftype file-path-designator ()
  `(or ,+file-path-host-type+ string))

(define-condition invalid-file-path (isys:iolib-error)
  ((path :initarg :path :reader invalid-file-path-path)
   (reason :initform nil :initarg :reason :reader invalid-file-path-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid file path: ~S."
                     (invalid-file-path-path condition))
             (when-let (reason (invalid-file-path-reason condition))
               (format stream "~%~A." reason)))))


;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(defconstant +directory-delimiter+
  #+unix    #\/
  #+windows #\\)

(defconstant +alternative-delimiter+
  #+unix    nil
  #+windows #\/)

(defconstant (+directory-delimiters+ :test #'equal)
  (list* +directory-delimiter+ +alternative-delimiter+))

(defconstant +execution-path-delimiter+
  #+unix    #\:
  #+windows #\;)

(declaim (special *default-file-path-defaults*))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

(defgeneric file-path (pathspec))

(defgeneric file-path-namestring (path))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defun file-path-host (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
        (%file-path-host-namestring path)
        (slot-value path 'host))))

(defun file-path-device (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
        (%file-path-device-namestring path)
        (slot-value path 'device))))

(defun file-path-components (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
       (%file-path-components-namestring
        path
        :print-dot t
        :trailing-delimiter (file-path-trailing-delimiter path))
       (slot-value path 'components))))

(defun split-root/nodes (dir)
  (if (eql :root (car dir))
      (values :root (cdr dir))
      (values nil   dir)))

(defun %file-path-directory (path)
  (let ((components (slot-value path 'components)))
    (multiple-value-bind (root nodes)
        (split-root/nodes components)
      (cons root (butlast nodes)))))

(defun file-path-directory (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
       (%file-path-directory-namestring path)
       (%file-path-directory path))))

(defun %file-path-file (path)
  (let ((components (slot-value path 'components)))
    (lastcar (nth-value 1 (split-root/nodes components)))))

(defun file-path-file (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
       (%file-path-file-namestring path)
       (%file-path-file path))))

(defun split-name/type (file)
  (let* ((dotpos (position #\. file :start 1 :from-end t)))
    (if (null dotpos)
        (values file nil)
        (values (subseq file 0 dotpos)
                (subseq file (1+ dotpos))))))

(defun file-path-file-name (pathspec)
  (let ((path (file-path pathspec)))
    (when-let (file (%file-path-file path))
     (nth-value 0 (split-name/type file)))))

(defun file-path-file-type (pathspec)
  (let ((path (file-path pathspec)))
    (when-let (file (%file-path-file path))
     (nth-value 1 (split-name/type file)))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun valid-component-types-p (components)
  (multiple-value-bind (root nodes)
      (split-root/nodes components)
    (and (member root '(nil :root))
         (every #'stringp nodes))))

(defmethod initialize-instance :after ((path file-path) &key components)
  (check-type components (and (not null) (satisfies valid-component-types-p)))
  (setf (slot-value path 'components) components)
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

(defmethod file-path ((path file-path))
  path)

(defmethod file-path (pathspec)
  (parse-file-path pathspec))

(defmethod file-path ((pathspec pathname))
  (parse-file-path (namestring pathspec)))

(defun make-file-path (&key (host nil hostp) (device nil devicep)
                       (components nil componentsp)
                       (defaults nil defaultsp) trailing-delimiter)
  (let ((defaults (and defaultsp (file-path defaults))))
    (make-instance '#.+file-path-host-type+
                   :host (cond (hostp     host)
                               (defaultsp (file-path-host defaults))
                               (t         (file-path-host
                                           *default-file-path-defaults*)))
                   :device (cond (devicep   device)
                                 (defaultsp (file-path-device defaults)))
                   :components (cond (componentsp components)
                                     (defaultsp   (file-path-components defaults)))
                   :trailing-delimiter trailing-delimiter)))

(defun merge-file-paths (pathspec &optional
                         (defaults *default-file-path-defaults*))
  (let ((path (file-path pathspec))
        (defaults (file-path defaults)))
    (make-instance '#.+file-path-host-type+
                   :host (or (file-path-host path)
                             (file-path-host defaults))
                   :device (or (file-path-device path)
                               (file-path-device defaults))
                   :components (if (absolute-p (file-path-components path))
                                   (file-path-components path)
                                   (append (file-path-components defaults)
                                           (file-path-components path)))
                   :trailing-delimiter (file-path-trailing-delimiter path))))


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
