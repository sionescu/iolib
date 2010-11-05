;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(in-package :iolib.base)

(defclass :iolib-muffled-source-file (asdf:cl-source-file) ())

(macrolet ((with-muffled-output (&body body)
             `(let ((*load-print* nil)
                    (*load-verbose* nil)
                    (*compile-print* nil)
                    (*compile-verbose* nil)
                    #+cmu (ext:*gc-verbose* nil))
                ,@body)))
  (defmethod asdf:perform :around ((o asdf:compile-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (call-next-method)))

  (defmethod asdf:perform :around ((o asdf:load-source-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (call-next-method))))

(defclass :iolib-source-file (:iolib-muffled-source-file) ())
