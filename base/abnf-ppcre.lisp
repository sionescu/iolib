;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(cl:defpackage :iolib-utils.abnf-pcre
    (:use :cl :alexandria)
  (:export #:define-abnf-syntax))

(in-package :iolib-utils.abnf-pcre)

(declaim (optimize (debug 3)))

(defparameter *default-non-terminals*
  (alexandria:alist-hash-table
   '((:digit . (:range #\0 #\9))
     (:hexdig . (:or (:range #\0 #\9) (:range #\a #\f) (:range #\A #\F)))
     (:alpha . (:or (:range #\a #\z) (:range #\A #\Z))))
   :test #'eq))

(defvar *non-terminals* nil)
(defvar *non-terminals-normalized* nil)

(defgeneric normalize-expression* (op expr))
(defgeneric encode-expression* (op args))

;;;
;;; Normalizing
;;;

(defun only-char-elems-p (expr)
  (loop :for e :in expr
        :always (or (and (stringp e) (= 1 (length e)))
                    (and (consp e) (eql :range (car e))))))

(defun normalize-expression (expr)
  (etypecase expr
    (cons (normalize-expression* (car expr) expr))
    (keyword (gethash expr *default-non-terminals*))
    (symbol (cond ((gethash expr *non-terminals-normalized*)
                   (gethash expr *non-terminals*))
                  (t
                   (setf (gethash expr *non-terminals*)
                         (normalize-expression (gethash expr *non-terminals*)))
                   (setf (gethash expr *non-terminals-normalized*) t))))
    ((or string character) expr)))

(defmacro define-normalizer (operator (&rest args) &body body)
  `(defmethod normalize-expression* ((op (eql ,operator)) ,@args)
     ,@body))

(define-normalizer :range (expr)
  (destructuring-bind (from to) (cdr expr)
    (check-type from character)
    (check-type to character)
    (assert (<= (char-code from) (char-code to)))
    `(:range ,from ,to)))

(define-normalizer :any (expr)
  (destructuring-bind (arg) (cdr expr)
    `(:repeat (0) ,(normalize-expression arg))))

(define-normalizer :req (expr)
  (destructuring-bind (arg) (cdr expr)
    `(:repeat (1) ,(normalize-expression arg))))

(define-normalizer :opt (expr)
  (destructuring-bind (arg) (cdr expr)
    `(:repeat (0 1) ,(normalize-expression arg))))

(define-normalizer :repeat (expr)
  (destructuring-bind (params args) (cdr expr)
    (ecase (car params)
      (:= (destructuring-bind (val) (cdr params)
            (check-type val unsigned-byte)
            `(:repeat (,val ,val) ,(normalize-expression args))))
      (:from (destructuring-bind (from &optional to) (cdr params)
               (check-type from unsigned-byte)
               (check-type to (or null unsigned-byte))
               `(:repeat (,from ,to) ,(normalize-expression args))))
      (:to (destructuring-bind (val) (cdr params)
             (check-type val unsigned-byte)
             `(:repeat (0 ,val) ,(normalize-expression args)))))))

(define-normalizer :or (expr)
  expr)

(define-normalizer :not (expr)
  (normalize-expression `(:or :not ,@(cdr expr))))

(define-normalizer :conc (expr)
  (list* :conc (mapcar #'normalize-expression (cdr expr))))

(defmethod normalize-expression* (op expr)
  (normalize-expression (list* :conc expr)))

;;;
;;; Encoding
;;;

(defun encode-expression (expr)
  (let ((e (normalize-expression expr)))
    (encode-expression* (car e) (cdr e))))

(defmacro define-encoder (operator (&rest args) &body body)
  `(defmethod encode-expression* ((op (eql ,operator)) ,@args)
     ,@body))

(define-encoder :range (args)
  (destructuring-bind (from to) args
    (format t "[~A-~A]" from to)))

(defmacro define-abnf-syntax (name &body clauses)
  )
