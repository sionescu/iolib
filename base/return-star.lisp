;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- RETURN* wrappers.
;;;

(in-package :iolib.base)

(cl:defmacro defun (name args &body body)
  `(,(find-right-symbol :defun :series)
     ,name ,args ,@(wrap-body-for-return-star body)))

(cl:defmacro defmethod (name method-qualifier args &body body)
  (cond
    ;; no method qualifier, this is actually the lambda-list
    ((listp method-qualifier)
     (setf body (cons args body)
           args method-qualifier)
     `(,(find-right-symbol :defmethod)
        ,name ,args ,@(wrap-body-for-return-star body)))
    (t
     `(,(find-right-symbol :defmethod) ,name
        ,method-qualifier ,args ,@(wrap-body-for-return-star body)))))

(cl:defmacro defmacro (name args &body body)
  `(,(find-right-symbol :defmacro)
     ,name ,args ,@(wrap-body-for-return-star body)))

(cl:defmacro define-compiler-macro (name args &body body)
  `(,(find-right-symbol :define-compiler-macro)
     ,name ,args ,@(wrap-body-for-return-star body)))

(cl:defun find-right-symbol (name &rest packages)
  (multiple-value-bind (symbol foundp)
      (if (eql (find-symbol (string name) *package*)
               (find-symbol (string name) :iolib.base))
          ;; NAME has been imported from IOLIB.UTILS, so we must
          ;; find a default somewhere else, defaulting to the CL package
          (find-symbol (string name) (find-right-package packages))
          ;; use the symbol named NAME from the *PACKAGE* or CL
          (find-symbol (string name) (find-right-package (package-name *package*))))
    (assert foundp (symbol) "Couldn't find any symbol as default for ~S" name)
    (values symbol)))

(cl:defun find-right-package (packages)
  (dolist (pkg (ensure-list packages) :common-lisp)
    (when (member pkg (package-use-list *package*)
                  :key #'package-name
                  :test #'string-equal)
      (return pkg))))

(cl:defun wrap-body-for-return-star (body)
  (multiple-value-bind (body declarations docstring)
      (parse-body body :documentation t)
    (with-gensyms (return-star-block)
      (remove-if
       #'null
       `(,docstring
         ,@declarations
         (block ,return-star-block
           (macrolet
               ((return* (value)
                  `(return-from ,',return-star-block ,value)))
             ,@body)))))))
