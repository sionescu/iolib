;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FFI designators.
;;;

(in-package :iolib.syscalls)

;;;; Type Designators

(defmacro define-designator (name cffi-type &body type-clauses)
  (let ((type `(quote (or ,@(mapcar #'car type-clauses))))
        (ctype (format-symbol t "~A-DESIGNATOR" name)))
    `(progn
       (deftype ,name () ,type)
       (defun ,name (,name)
         (etypecase ,name
           ,@type-clauses))
       (define-foreign-type ,ctype ()
         ()
         (:simple-parser ,ctype)
         (:actual-type ,cffi-type))
       (defmethod expand-to-foreign (value (type ,ctype))
         `(convert-to-foreign
           (let ((,',name ,value))
             (etypecase ,',name ,@',type-clauses))
           ,',cffi-type)))))

(declaim (inline native-namestring))
(defun native-namestring (pathname)
  (cffi-sys:native-namestring pathname))

;;; NATIVE-NAMESTRING should take care of complaining when FILENAME
;;; is wild but I don't think it does on all Lisps, so let's check it
;;; explicitly.
(define-designator filename :string
  (pathname (when (wild-pathname-p filename)
              (syscall-error "Pathname is wild: ~S." filename))
            (native-namestring (translate-logical-pathname filename)))
  (string filename))

(define-designator pointer-or-nil :pointer
  (null (null-pointer))
  (foreign-pointer pointer-or-nil))

(define-designator bool :int
  (null 0)
  (t    1))
