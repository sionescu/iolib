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

;;; HASH TABLE mapping error codes to symbols denoting
;;; subtypes of POSIX-ERROR.
(defparameter *posix-error-map* (make-hash-table :test 'eql))

(declaim (inline get-posix-error-condition))
(defun get-posix-error-condition (errno)
  (gethash errno *posix-error-map*))

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
                    (setf (gethash ,code *posix-error-map*) ',cond-name)))))))
  (define-posix-errors
    #.(foreign-enum-keyword-list 'errno-values)))

;;; Instantiates a subclass of POSIX-ERROR matching ERR
;;; ERR must be either an integer denoting an ERRNO value.
(defun make-posix-error (errno fd)
  (debug-only* (assert (integerp errno)))
  (let ((error-keyword (foreign-enum-keyword 'errno-values errno :errorp nil)))
    (unless error-keyword
      (bug "A non-existent ~A syscall error has been signaled: ~A, ~A"
           'errno-values (or error-keyword :unknown) errno))
    (make-condition (get-posix-error-condition errno)
                    :handle fd)))

(declaim (inline posix-error))
(defun signal-posix-error (&optional (errno (%sys-errno)) fd)
  (error (make-posix-error errno fd)))

(defun signal-posix-error-kw (error-keyword fd)
  (let ((errno (foreign-enum-value 'errno-values error-keyword :errorp nil)))
    (unless error-keyword
      (bug "A non-existent ~A syscall error has been signaled: ~A, ~A"
           'errno-values error-keyword errno))
    (error (make-posix-error errno fd))))
