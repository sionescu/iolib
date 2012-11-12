;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Creating aliases in CL namespaces
;;;

(in-package :iolib.base)

(defvar *namespaces* nil)

(defmacro defalias (alias original)
  (destructuring-bind (namespace new-name)
      alias
    (assert (member namespace *namespaces*) (namespace)
            "Namespace ~A does not exist" namespace)
    (make-alias namespace original new-name)))

(defmacro defnamespace (namespace &optional docstring)
  (check-type namespace symbol)
  (check-type docstring (or null string))
  `(progn
     (pushnew ',namespace *namespaces*)
     (handler-bind ((warning #'muffle-warning))
       (setf (documentation ',namespace 'namespace) ,docstring))))

(defgeneric make-alias (namespace original alias))

(defnamespace function
  "The namespace of ordinary and generic functions.")

(defmethod make-alias ((namespace (eql 'function))
                       original alias)
  `(progn
     (setf (fdefinition ',alias)
           (fdefinition ',original))
     (setf (documentation ',alias 'function)
           (documentation ',original 'function))))

(defnamespace macro
  "The namespace of macros.")

(defmethod make-alias ((namespace (eql 'macro))
                       original alias)
  `(progn
     (setf (macro-function ',alias)
           (macro-function ',original))
     (setf (documentation ',alias 'function)
           (documentation ',original 'function))))

(defnamespace special
  "The namespace of special variables.")

(defmethod make-alias ((namespace (eql 'special))
                       original alias)
  `(progn
     (define-symbol-macro ,alias ,original)
     (setf (documentation ',alias 'variable)
           (documentation ',original 'variable))))

(defnamespace constant
  "The namespace of special variables.")

(defmethod make-alias ((namespace (eql 'constant))
                       original alias)
  `(progn
     (define-symbol-macro ,alias ,original)
     (setf (documentation ',alias 'variable)
           (documentation ',original 'variable))))
