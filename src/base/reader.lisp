;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
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

(define-condition unknown-literal-read-syntax (reader-error)
  ((name :initarg name :accessor unknown-literal-read-syntax-name))
  (:report (lambda (s c)
             (format s "Unknown literal read syntax: ~S"
                     (unknown-literal-read-syntax-name c)))))

(defconstant +read-literal-dispatch-char+ #\#)
(defconstant +read-literal-sub-char+ #\/)

(defun read-literal-dispatcher (stream char arg)
  (declare (ignore char arg))
  (let* ((literal-kind
          (with-output-to-string (s)
            (loop :for c := (read-char stream t nil t) :do
                  (cond
                    ((char= c +read-literal-sub-char+)
                     (loop-finish))
                    (t (write-char c s))))))
         (actual-reader
          (getf (symbol-plist (read-from-string literal-kind))
                'read-literal-fn)))
    (if (functionp actual-reader)
        (funcall actual-reader stream)
        (error 'unknown-literal-read-syntax
               :stream stream
               :name actual-reader))))

(set-dispatch-macro-character +read-literal-dispatch-char+
                              +read-literal-sub-char+
                              'read-literal-dispatcher)

(defmacro define-literal-reader (name (stream) &body body)
  `(setf (getf (symbol-plist ',name) 'read-literal-fn)
         (lambda (,stream) ,@body)))
