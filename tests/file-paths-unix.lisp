;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.pathnames test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.pathnames)

(defmacro is-file-path (path (directory file))
  (with-gensyms (p)
    `(is-true
      (let ((,p ,path))
        (and (eql (file-path-host ,p) :unspecific)
             (eql (file-path-device ,p) :unspecific)
             (eql (car (file-path-directory ,p)) ,(car directory))
             (and (= (length ',directory)
                     (length (file-path-directory ,p)))
                  (every #'ustring=
                         (cdr (file-path-directory ,p))
                         ',(cdr directory)))
             (ustring= (file-path-file ,p) ',file))))))

(test file-path.null.1
  (signals invalid-file-path
    (file-path "")))

(test file-path.null.2
  (signals invalid-file-path
    (file-path "" :as-directory t)))

(test file-path.null.3
  (signals invalid-file-path
    (file-path "" :expand-user t)))

(test file-path.null.4
  (signals invalid-file-path
    (file-path "" :as-directory t :expand-user t)))

(test file-path.root.1
  (is-file-path (file-path "/")
                ((:absolute) nil)))

(test file-path.root.2
  (is-file-path (file-path "/" :as-directory t)
                ((:absolute) nil)))

(test file-path.root.3
  (is-file-path (file-path "/" :expand-user t)
                ((:absolute) nil)))

(test file-path.root.4
  (is-file-path (file-path "/" :as-directory t :expand-user t)
                ((:absolute) nil)))

(test file-path.relative.1
  (is-file-path (file-path "a")
                ((:relative) "a")))

(test file-path.relative.2
  (is-file-path (file-path "a" :as-directory t)
                ((:relative) "a")))

(test file-path.relative.3
  (is-file-path (file-path "a" :expand-user t)
                ((:relative) "a")))

(test file-path.relative.4
  (is-file-path (file-path "a" :as-directory t :expand-user t)
                ((:relative) "a")))

(test file-path.relative.5
  (is-file-path (file-path "a/")
                ((:relative) "a")))

(test file-path.relative.6
  (is-file-path (file-path "a/" :as-directory t)
                ((:relative) "a")))

(test file-path.relative.7
  (is-file-path (file-path "a/" :expand-user t)
                ((:relative) "a")))

(test file-path.relative.8
  (is-file-path (file-path "a/" :as-directory t :expand-user t)
                ((:relative) "a")))

(test file-path.relative.9
  (is-file-path (file-path "a/b")
                ((:relative "a") "b")))

(test file-path.relative.10
  (is-file-path (file-path "a/b" :as-directory t)
                ((:relative "a") "b")))

(test file-path.relative.11
  (is-file-path (file-path "a/b" :expand-user t)
                ((:relative "a") "b")))

(test file-path.relative.12
  (is-file-path (file-path "a/b" :as-directory t :expand-user t)
                ((:relative "a") "b")))

(test file-path.absolute.1
  (is-file-path (file-path "/a")
                ((:absolute) "a")))

(test file-path.absolute.2
  (is-file-path (file-path "/a" :as-directory t)
                ((:absolute) "a")))

(test file-path.absolute.3
  (is-file-path (file-path "/a" :expand-user t)
                ((:absolute) "a")))

(test file-path.absolute.4
  (is-file-path (file-path "/a" :as-directory t :expand-user t)
                ((:absolute) "a")))

(test file-path.absolute.5
  (is-file-path (file-path "/a/")
                ((:absolute) "a")))

(test file-path.absolute.6
  (is-file-path (file-path "/a/" :as-directory t)
                ((:absolute) "a")))

(test file-path.absolute.7
  (is-file-path (file-path "/a/" :expand-user t)
                ((:absolute) "a")))

(test file-path.absolute.8
  (is-file-path (file-path "/a/" :as-directory t :expand-user t)
                ((:absolute) "a")))

(test file-path.absolute.9
  (is-file-path (file-path "/a/b")
                ((:absolute "a") "b")))

(test file-path.absolute.10
  (is-file-path (file-path "/a/b" :as-directory t)
                ((:absolute "a") "b")))

(test file-path.absolute.11
  (is-file-path (file-path "/a/b" :expand-user t)
                ((:absolute "a") "b")))

(test file-path.absolute.12
  (is-file-path (file-path "/a/b" :as-directory t :expand-user t)
                ((:absolute "a") "b")))

(test file-path.expand-user.1
  (is-file-path (file-path "~root" :expand-user nil)
                ((:relative) "~root")))

(test file-path.expand-user.2
  (is-file-path (file-path "~root" :as-directory t :expand-user nil)
                ((:relative) "~root")))

(test file-path.expand-user.3
  (is-file-path (file-path "~root" :expand-user t)
                ((:absolute) "root")))

(test file-path.expand-user.4
  (is-file-path (file-path "~root" :as-directory t :expand-user t)
                ((:absolute) "root")))

(test file-path.expand-user.5
  (is-file-path (file-path "/~root")
                ((:absolute) "~root")))

(test file-path.expand-user.6
  (is-file-path (file-path "/~root" :as-directory t)
                ((:absolute) "~root")))

(test file-path.expand-user.7
  (is-file-path (file-path "/~root" :expand-user t)
                ((:absolute) "~root")))

(test file-path.expand-user.8
  (is-file-path (file-path "/~root" :as-directory t :expand-user t)
                ((:absolute) "~root")))

(test file-path.expand-user.9
  (is-file-path (file-path "~root/a" :expand-user nil)
                ((:relative "~root") "a")))

(test file-path.expand-user.10
  (is-file-path (file-path "~root/a" :as-directory t :expand-user nil)
                ((:relative "~root") "a")))

(test file-path.expand-user.11
  (is-file-path (file-path "~root/a" :expand-user t)
                ((:absolute "root") "a")))

(test file-path.expand-user.12
  (is-file-path (file-path "~root/a" :as-directory t :expand-user t)
                ((:absolute "root") "a")))


(test file-path.namestring.1
  (is (string= "/" (file-path-namestring (file-path "/")))))

(test file-path.namestring.2
  (is (string= "/." (file-path-namestring (file-path "/.")))))

(test file-path.namestring.3
  (is (string= "/.." (file-path-namestring (file-path "/..")))))

(test file-path.namestring.4
  (is (string= "." (file-path-namestring (file-path ".")))))

(test file-path.namestring.5
  (is (string= "./" (file-path-namestring (file-path "./")))))

(test file-path.namestring.6
  (is (string= "../." (file-path-namestring (file-path "../.")))))

(test file-path.namestring.7
  (is (string= ".././" (file-path-namestring (file-path ".././")))))

(test file-path.namestring.8
  (is (string= "../.." (file-path-namestring (file-path "../..")))))

(test file-path.namestring.9
  (is (string= "a/./b" (file-path-namestring (file-path "a/./b")))))

(test file-path.namestring.10
  (is (string= "a/../b" (file-path-namestring (file-path "a/../b")))))
