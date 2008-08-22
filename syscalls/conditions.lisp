;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib.syscalls)

;;;-----------------------------------------------------------------------------
;;; System Errors
;;;-----------------------------------------------------------------------------

(define-condition system-error (error)
  ((code :initarg :code :reader code-of
         :documentation "Numeric error code, or NIL.")
   (identifier :initarg :identifier :reader identifier-of
               :documentation "Keyword identifier, or NIL.")
   (message :initarg :message :reader message-of
            :documentation "Error description."))
  (:default-initargs :code nil
                     :identifier :unknown-error)
  (:documentation
   "Base class for errors signalled by IOlib low-level functions."))

(defun system-error (control-string &rest args)
  (error 'system-error :message (format nil "~?" control-string args)))

(define-condition syscall-error (system-error)
  ()
  (:documentation "Base class for syscall errors."))

(defun syscall-error (control-string &rest args)
  (error 'syscall-error :message (format nil "~?" control-string args)))


;;;-----------------------------------------------------------------------------
;;; I/O Poll Errors
;;;-----------------------------------------------------------------------------

(define-condition poll-error (system-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Error caught while polling: ~A"
                     (message-of condition))))
  (:documentation
   "Signaled when an error occurs while polling for I/O readiness
of a file descriptor."))

(define-condition poll-timeout (condition)
  ((event-type :initarg :event-type :reader event-type-of))
  (:report (lambda (condition stream)
             (format stream "Timeout occurred while polling for event ~S"
                     (event-type-of condition))))
  (:documentation
   "Signaled when a timeout occurs while polling for I/O readiness
of a file descriptor."))


;;;-----------------------------------------------------------------------------
;;; Repeat upon conditions
;;;-----------------------------------------------------------------------------

(defmacro repeat-decreasing-timeout
    ((timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless (find timeout-var (flatten body))
    (warn "You probably want to use ~S inside the body ~A" timeout-var body))
  (unless blockp (setf block-name (gensym "BLOCK")))
  (with-unique-names (deadline temp-timeout)
    `(let* ((,timeout-var ,timeout)
            (,deadline (when ,timeout-var
                         (+ ,timeout-var (get-monotonic-time)))))
       (loop :named ,block-name :do
         ,@body
           (when ,deadline
             (let ((,temp-timeout (- ,deadline (get-monotonic-time))))
               (setf ,timeout-var
                     (if (plusp ,temp-timeout)
                         ,temp-timeout
                         0))))))))

(defmacro repeat-upon-condition-decreasing-timeout
    (((&rest conditions) timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless blockp (setf block-name (gensym "BLOCK")))
  `(repeat-decreasing-timeout (,timeout-var ,timeout ,block-name)
     (ignore-some-conditions ,conditions
       (return-from ,block-name (progn ,@body)))))
