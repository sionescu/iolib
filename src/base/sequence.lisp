;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Sequence utils
;;;

(in-package :iolib.base)

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (check-type ,start unsigned-byte "a non-negative integer")
       (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
       (unless ,end
         (setf ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(defun join (connector &rest strings)
  (let ((c (string connector)))
    (concatenate 'string (car strings)
                 (reduce (lambda (str1 str2)
                           (concatenate 'string str1 c str2))
                         (cdr strings)
                         :initial-value ""))))
