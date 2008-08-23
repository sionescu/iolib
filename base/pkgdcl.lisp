;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.base
  (:use #:common-lisp :alexandria)
  (:shadow #:defun #:defmethod #:defmacro #:define-compiler-macro)
  (:export
   ;; Conditions
   #:bug
   ;; RETURN*
   #:return* #:lambda* #:defun #:defmethod
   #:defmacro #:define-compiler-macro
   ;; DEFOBSOLETE
   #:defobsolete
   #:deprecation-warning
   #:deprecation-warning-function-name
   #:deprecation-warning-type
   #:deprecation-warning-reason
   ;; Reader utils
   #:define-syntax
   #:enable-reader-macro #:enable-reader-macro*
   #:disable-reader-macro #:disable-reader-macro*
   ;; SPLIT-SEQUENCE
   #:split-sequence #:split-sequence-if #:split-sequence-if-not
   ;; Misc
   #:function-name #:function-name-p
   #:check-bounds
   ;; Matching
   #:multiple-value-case #:flags-case
   ;; Time
   #:decode-timeout #:normalize-timeout #:clamp-timeout))

(flet ((gather-external-symbols (&rest packages)
         (let ((symbols (make-hash-table :test #'eq)))
           (with-package-iterator (iterator packages :external)
             (loop (multiple-value-bind (morep symbol) (iterator)
                     (unless morep (return))
                     (setf (gethash (alexandria:ensure-symbol symbol :iolib.base) symbols) t))))
           (alexandria:hash-table-keys symbols))))
  (export (gather-external-symbols :common-lisp :alexandria :iolib.base)
          :iolib.base))
