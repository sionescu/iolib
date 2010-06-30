;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(in-package :iolib.asdf)

(defclass muffled-source-file (asdf:cl-source-file) ())

(macrolet ((with-muffled-output (&body body)
             `(let ((*load-print* nil)
                    (*load-verbose* t)
                    (*compile-print* nil)
                    (*compile-verbose* t)
                    #+cmu (ext:*gc-verbose* nil))
                ,@body)))
  (defmethod asdf:perform :around ((o asdf:compile-op) (c muffled-source-file))
    (with-muffled-output
      (call-next-method)))

  (defmethod asdf:perform :around ((o asdf:load-source-op) (c muffled-source-file))
    (with-muffled-output
      (call-next-method))))

(defclass iolib-source-file (muffled-source-file) ())

(import 'iolib-source-file (find-package :asdf))
