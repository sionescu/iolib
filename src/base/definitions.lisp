;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various definers
;;;

(in-package :iolib.base)

(defmacro defconstant (name value &optional documentation)
  (destructuring-bind (name &key (test ''eql))
      (ensure-list name)
    (macroexpand-1
     `(define-constant ,name ,value
        :test ,test
        ,@(when documentation `(:documentation ,documentation))))))
