;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *NIX-specific error conditions.
;;;

(in-package :iolib.syscalls)

;;;-------------------------------------------------------------------------
;;; POSIX Syscall Errors
;;;-------------------------------------------------------------------------

(define-condition posix-error (syscall-error)
  ()
  (:documentation
   "POSIX-ERRORs are signalled whenever ERRNO is set by a POSIX call."))

;;; HASH TABLE mapping keywords (such as :EAGAIN) to symbols denoting
;;; subtypes of POSIX-ERROR.
(defparameter *posix-error-map* (make-hash-table :test #'eq))

(defun get-posix-error-condition (keyword)
  (gethash keyword *posix-error-map*))

;;; Define an error condition for each ERRNO value defined in the
;;; ERRNO-VALUES enum type and populate *POSIX-ERROR-MAP*.
(macrolet
    ((define-posix-errors (keywords)
       `(progn
          ,@(loop :for kw :in keywords :collect
               (let ((cond-name (intern (symbol-name kw)))
                     (code (foreign-enum-value 'errno-values kw)))
                 `(progn
                    (define-condition ,cond-name (posix-error) ()
                      (:default-initargs :code ,code :identifier ,kw))
                    (setf (gethash ,kw *posix-error-map*) ',cond-name)))))))
  (define-posix-errors
      #.(foreign-enum-keyword-list 'errno-values)))

;;; Instantiates a subclass of POSIX-ERROR matching ERR or a plain
;;; POSIX-ERROR if no matching subclass is found.  ERR can be either a
;;; keyword or an integer both denoting an ERRNO value.
(defun make-posix-error (err)
  (multiple-value-bind (error-keyword error-code)
      (typecase err
        (keyword (values err (foreign-enum-value 'errno-values err :errorp nil)))
        (integer (values (foreign-enum-keyword 'errno-values err :errorp nil) err))
        (t (bug "Non-valid error-designator: ~A" err)))
    (unless (and error-keyword error-code)
      (bug "A non-existent ~A syscall error has been signaled: ~A, ~A"
           'errno-values (or error-keyword :unknown) error-code))
    (make-condition (get-posix-error-condition error-keyword))))

(declaim (inline posix-error))
(defun signal-posix-error (&optional (errno (%sys-errno)))
  (error (make-posix-error errno)))
