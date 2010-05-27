;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
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

(defmacro shrink-vector (str size)
  #+allegro `(excl::.primcall 'sys::shrink-svector ,str ,size)
  #+cmu `(lisp::shrink-vector ,str ,size)
  #+lispworks `(system::shrink-vector$vector ,str ,size)
  #+sbcl `(sb-kernel:shrink-vector ,str ,size)
  #+scl `(common-lisp::shrink-vector ,str ,size)
  #-(or allegro cmu lispworks sbcl scl) `(subseq ,str 0 ,size))
