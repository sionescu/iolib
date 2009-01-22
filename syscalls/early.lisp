;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Early definitions.
;;;

(in-package :iolib.syscalls)

;;;; Sizes of Standard Types

(defconstant size-of-char (foreign-type-size :char))
(defconstant size-of-int (foreign-type-size :int))
(defconstant size-of-long (foreign-type-size :long))
(defconstant size-of-long-long (foreign-type-size :long-long))
(defconstant size-of-pointer (foreign-type-size :pointer))
(defconstant size-of-short (foreign-type-size :short))


;;; Error predicate that always returns NIL.  Not actually used
;;; because the RETURN-WRAPPER optimizes this call away.
(defun never-fails (errcode)
  (declare (ignore errcode))
  nil)

;;; NOTE: This is a pretty neat type that probably deserves to be
;;; included in CFFI. --luis
;;;
;;; This type is used by DEFSYSCALL to automatically check for errors
;;; using the ERROR-PREDICATE function which is passed the foreign
;;; function's return value (after going through RETURN-FILTER).  If
;;; ERROR-PREDICATE returns true, ERROR-GENERATOR is invoked.  See the
;;; RETURN-WRAPPER parse method and type translation.
(define-foreign-type return-wrapper ()
  ((error-predicate :initarg :error-predicate :reader error-predicate-of)
   (return-filter :initarg :return-filter :reader return-filter-of)
   (error-generator :initarg :error-generator :reader error-generator-of)
   (base-type :initarg :base-type :reader base-type-of)))

(define-parse-method return-wrapper
    (base-type &key error-predicate (return-filter 'identity) error-generator)
  ;; pick a default error-predicate
  (unless error-predicate
    (case base-type
      (:string
       (setf error-predicate '(lambda (s) (not (stringp s)))))
      (t
       (case (cffi::canonicalize-foreign-type base-type)
         (:pointer
          (setf error-predicate 'null-pointer-p))
         ((:char :short :int :long :long-long)
          (setf error-predicate 'minusp))
         ;; FIXME: go here if the canonical type is unsigned.
         ((:unsigned-char :unsigned-short :unsigned-int
           :unsigned-long :unsigned-long-long :void)
          (setf error-predicate 'never-fails))
         (t
          (error "Could not choose an error-predicate function."))))))
  (unless (or (eql 'never-fails error-predicate) error-generator)
    (error "Function can fail but no error-generator suplied."))
  (make-instance 'return-wrapper
                 :actual-type base-type
                 :base-type base-type
                 :error-predicate error-predicate
                 :return-filter return-filter
                 :error-generator error-generator))

;;; This type translator sets up the appropriate calls to
;;; RETURN-FILTER, ERROR-PREDICATE and ERROR-GENERATOR around the
;;; foreign function call.
(defmethod expand-from-foreign (value (type return-wrapper))
  (if (and (eql 'identity (return-filter-of type))
           (eql 'never-fails (error-predicate-of type)))
      value
      (with-gensyms (block)
        `(block ,block
           (tagbody :restart
              (let ((r (convert-from-foreign ,value ',(base-type-of type))))
                ,(let ((return-exp
                        (if (eql 'identity (return-filter-of type))
                            'r
                            `(,(return-filter-of type) r))))
                      `(return-from ,block
                         ,(if (eql 'never-fails (error-predicate-of type))
                              `return-exp
                              `(if (,(error-predicate-of type) r)
                                   (,(error-generator-of type) r)
                                   ,return-exp))))))))))


(defun foreign-name (spec &optional varp)
  (declare (ignore varp))
  (check-type spec list)
  (destructuring-bind (first second) spec
    (etypecase first
      ((or string cons)
       (foreign-name (list second (ensure-list first))))
      (symbol
       (setf second (ensure-list second))
       (assert (every #'stringp second))
       (loop :for sym :in second
             :if (foreign-symbol-pointer sym) :do (return sym)
             :finally
             (error "None of these foreign symbols is defined: ~{~S~^, ~}"
                    second))))))

(defun parse-name-and-options (spec &optional varp)
  (values (cffi::lisp-name spec varp)
          (foreign-name spec varp)
          (cffi::foreign-options spec varp)))


(defmacro defentrypoint (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(defmacro defcfun* (name-and-opts return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (parse-name-and-options name-and-opts)
    `(progn
       (declaim (inline ,lisp-name))
       (defcfun (,c-name ,lisp-name ,@options) ,return-type
         ,@args))))

(defmacro signal-posix-error/restart (ret)
  `(if (= eintr (%sys-errno))
       (go :restart)
       (signal-posix-error ,ret)))

(defmacro defsyscall (name-and-opts return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (parse-name-and-options name-and-opts)
    `(progn
       (declaim (inline ,lisp-name))
       (defcfun (,c-name ,lisp-name ,@options)
           (return-wrapper ,return-type :error-generator signal-posix-error)
         ,@args))))

(defmacro defsyscall* (name-and-opts return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (parse-name-and-options name-and-opts)
    `(progn
       (declaim (inline ,lisp-name))
       (defcfun (,c-name ,lisp-name ,@options)
           (return-wrapper ,return-type :error-generator signal-posix-error/restart)
         ,@args))))
