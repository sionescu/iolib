;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.pathnames test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.pathnames)

(defmacro is-file-path (path (&rest components))
  (with-gensyms (p)
    `(is-true
      (let ((,p ,path))
        (and (eql (file-path-host ,p) :unspecific)
             (eql (file-path-device ,p) :unspecific)
             (and (= (length ',components)
                     (length (file-path-components ,p)))
                  (every #'(lambda (x y)
                             (typecase x
                               (keyword (eql x y))
                               (ustring (ustring= x y))))
                         (file-path-components ,p)
                         ',components)))))))


(test file-path.null.1
  (signals invalid-file-path
    (parse-file-path "")))

(test file-path.null.2
  (signals invalid-file-path
    (parse-file-path "" :as-directory t)))

(test file-path.null.3
  (signals invalid-file-path
    (parse-file-path "" :expand-user t)))

(test file-path.null.4
  (signals invalid-file-path
    (parse-file-path "" :as-directory t :expand-user t)))


(test file-path.root.1
  (is-file-path (parse-file-path "/")
                (:root)))

(test file-path.root.2
  (is-file-path (parse-file-path "/" :as-directory t)
                (:root)))

(test file-path.root.3
  (is-file-path (parse-file-path "/" :expand-user t)
                (:root)))

(test file-path.root.4
  (is-file-path (parse-file-path "/" :as-directory t :expand-user t)
                (:root)))


(test file-path.relative.1
  (is-file-path (parse-file-path "a")
                ("a")))

(test file-path.relative.2
  (is-file-path (parse-file-path "a" :as-directory t)
                ("a")))

(test file-path.relative.3
  (is-file-path (parse-file-path "a" :expand-user t)
                ("a")))

(test file-path.relative.4
  (is-file-path (parse-file-path "a" :as-directory t :expand-user t)
                ("a")))

(test file-path.relative.5
  (is-file-path (parse-file-path "a/")
                ("a")))

(test file-path.relative.6
  (is-file-path (parse-file-path "a/" :as-directory t)
                ("a")))

(test file-path.relative.7
  (is-file-path (parse-file-path "a/" :expand-user t)
                ("a")))

(test file-path.relative.8
  (is-file-path (parse-file-path "a/" :as-directory t :expand-user t)
                ("a")))

(test file-path.relative.9
  (is-file-path (parse-file-path "a/b")
                ("a" "b")))

(test file-path.relative.10
  (is-file-path (parse-file-path "a/b" :as-directory t)
                ("a" "b")))

(test file-path.relative.11
  (is-file-path (parse-file-path "a/b" :expand-user t)
                ("a" "b")))

(test file-path.relative.12
  (is-file-path (parse-file-path "a/b" :as-directory t :expand-user t)
                ("a" "b")))


(test file-path.absolute.1
  (is-file-path (parse-file-path "/a")
                (:root "a")))

(test file-path.absolute.2
  (is-file-path (parse-file-path "/a" :as-directory t)
                (:root "a")))

(test file-path.absolute.3
  (is-file-path (parse-file-path "/a" :expand-user t)
                (:root "a")))

(test file-path.absolute.4
  (is-file-path (parse-file-path "/a" :as-directory t :expand-user t)
                (:root "a")))

(test file-path.absolute.5
  (is-file-path (parse-file-path "/a/")
                (:root "a")))

(test file-path.absolute.6
  (is-file-path (parse-file-path "/a/" :as-directory t)
                (:root "a")))

(test file-path.absolute.7
  (is-file-path (parse-file-path "/a/" :expand-user t)
                (:root "a")))

(test file-path.absolute.8
  (is-file-path (parse-file-path "/a/" :as-directory t :expand-user t)
                (:root "a")))

(test file-path.absolute.9
  (is-file-path (parse-file-path "/a/b")
                (:root "a" "b")))

(test file-path.absolute.10
  (is-file-path (parse-file-path "/a/b" :as-directory t)
                (:root "a" "b")))

(test file-path.absolute.11
  (is-file-path (parse-file-path "/a/b" :expand-user t)
                (:root "a" "b")))

(test file-path.absolute.12
  (is-file-path (parse-file-path "/a/b" :as-directory t :expand-user t)
                (:root "a" "b")))


(test file-path.expand-user.1
  (is-file-path (parse-file-path "~root" :expand-user nil)
                ("~root")))

(test file-path.expand-user.2
  (is-file-path (parse-file-path "~root" :as-directory t :expand-user nil)
                ("~root")))

(test file-path.expand-user.3
  (is-file-path (parse-file-path "~root" :expand-user t)
                (:root "root")))

(test file-path.expand-user.4
  (is-file-path (parse-file-path "~root" :as-directory t :expand-user t)
                (:root "root")))

(test file-path.expand-user.5
  (is-file-path (parse-file-path "/~root")
                (:root "~root")))

(test file-path.expand-user.6
  (is-file-path (parse-file-path "/~root" :as-directory t)
                (:root "~root")))

(test file-path.expand-user.7
  (is-file-path (parse-file-path "/~root" :expand-user t)
                (:root "~root")))

(test file-path.expand-user.8
  (is-file-path (parse-file-path "/~root" :as-directory t :expand-user t)
                (:root "~root")))

(test file-path.expand-user.9
  (is-file-path (parse-file-path "~root/a" :expand-user nil)
                ("~root" "a")))

(test file-path.expand-user.10
  (is-file-path (parse-file-path "~root/a" :as-directory t :expand-user nil)
                ("~root" "a")))

(test file-path.expand-user.11
  (is-file-path (parse-file-path "~root/a" :expand-user t)
                (:root "root" "a")))

(test file-path.expand-user.12
  (is-file-path (parse-file-path "~root/a" :as-directory t :expand-user t)
                (:root "root" "a")))


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


(test file-path.name.1
  (is (eql nil (file-path-name (file-path "/")))))

(test file-path.name.2
  (is (string= "a" (file-path-name (file-path "a")))))

(test file-path.name.3
  (is (string= "a" (file-path-name (file-path "a.")))))

(test file-path.name.4
  (is (string= ".a" (file-path-name (file-path ".a")))))

(test file-path.name.4
  (is (string= "a" (file-path-name (file-path "a.b")))))


(test file-path.type.1
  (is (eql nil (file-path-type (file-path "/")))))

(test file-path.type.2
  (is (eql nil (file-path-type (file-path "a")))))

(test file-path.type.3
  (is (string= "" (file-path-type (file-path "a.")))))

(test file-path.type.4
  (is (string= nil (file-path-type (file-path ".a")))))

(test file-path.type.4
  (is (string= "b" (file-path-type (file-path "a.b")))))
