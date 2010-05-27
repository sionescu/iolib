;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Reader utils
;;;

(in-package :iolib.base)

;; Reader macros

(defgeneric enable-reader-macro* (name))

(defgeneric disable-reader-macro* (name))

(defmacro enable-reader-macro (name)
  `(eval-when (:compile-toplevel)
     (enable-reader-macro* ,name)))

(defmacro disable-reader-macro (name)
  `(eval-when (:compile-toplevel)
     (disable-reader-macro* ,name)))

(defun save-old-readtable (symbol readtable)
  (setf (getf (symbol-plist symbol) 'old-readtable) readtable))

(defun get-old-readtable (symbol)
  (getf (symbol-plist symbol) 'old-readtable))

(defmethod enable-reader-macro* :before ((name symbol))
  (save-old-readtable name *readtable*)
  (setf *readtable* (copy-readtable)))

(defmethod disable-reader-macro* ((name symbol))
  (assert (readtablep (get-old-readtable name)))
  (setf *readtable* (get-old-readtable name))
  (save-old-readtable name nil))

(defmacro define-syntax (name &body body)
  `(defmethod enable-reader-macro* ((name (eql ',name)))
     ,@body))


;; Literal object dispatcher

(defconstant +read-literal-dispatch-char+ #\#)
(defconstant +read-literal-sub-char+ #\/)

(defun read-literal-dispatcher (stream char arg)
  (declare (ignore char arg))
  (let* ((literal-syntax-name
          (with-output-to-string (s)
            (loop :for c := (read-char stream t nil t)
                  :do (if (char= c +read-literal-sub-char+)
                          (loop-finish)
                          (write-char c s)))))
         (literal-reader
          (getf (symbol-plist (read-from-string literal-syntax-name))
                'read-literal-fn)))
    (if (functionp literal-reader)
        (funcall literal-reader stream)
        (error 'unknown-literal-syntax
               :stream stream
               :name literal-syntax-name))))

(set-dispatch-macro-character +read-literal-dispatch-char+
                              +read-literal-sub-char+
                              'read-literal-dispatcher)

(defmacro define-literal-reader (name (stream) &body body)
  `(setf (getf (symbol-plist ',name) 'read-literal-fn)
         (lambda (,stream) ,@body)))
