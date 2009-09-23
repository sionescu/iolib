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
                               (string (string= x y))))
                         (file-path-components ,p)
                         ',components)))))))


(test file-path.null.1
  (signals invalid-file-path
    (parse-file-path "")))

(test file-path.null.2
  (signals invalid-file-path
    (parse-file-path "" :expand-user t)))


(test file-path.root.1
  (is-file-path (parse-file-path "/")
                (:root)))

(test file-path.root.2
  (is-file-path (parse-file-path "/" :expand-user t)
                (:root)))


(test file-path.relative.1
  (is-file-path (parse-file-path "a")
                ("a")))

(test file-path.relative.2
  (is-file-path (parse-file-path "a" :expand-user t)
                ("a")))

(test file-path.relative.3
  (is-file-path (parse-file-path "a/")
                ("a")))

(test file-path.relative.4
  (is-file-path (parse-file-path "a/" :expand-user t)
                ("a")))

(test file-path.relative.5
  (is-file-path (parse-file-path "a/b")
                ("a" "b")))

(test file-path.relative.6
  (is-file-path (parse-file-path "a/b" :expand-user t)
                ("a" "b")))


(test file-path.absolute.1
  (is-file-path (parse-file-path "/a")
                (:root "a")))

(test file-path.absolute.2
  (is-file-path (parse-file-path "/a" :expand-user t)
                (:root "a")))

(test file-path.absolute.3
  (is-file-path (parse-file-path "/a/")
                (:root "a")))

(test file-path.absolute.4
  (is-file-path (parse-file-path "/a/" :expand-user t)
                (:root "a")))

(test file-path.absolute.5
  (is-file-path (parse-file-path "/a/b")
                (:root "a" "b")))

(test file-path.absolute.6
  (is-file-path (parse-file-path "/a/b" :expand-user t)
                (:root "a" "b")))


(test file-path.expand-user.1
  (is-file-path (parse-file-path "~root" :expand-user nil)
                ("~root")))

(test file-path.expand-user.2
  (is-file-path (parse-file-path "~root" :expand-user t)
                (:root "root")))

(test file-path.expand-user.3
  (is-file-path (parse-file-path "/~root")
                (:root "~root")))

(test file-path.expand-user.4
  (is-file-path (parse-file-path "/~root" :expand-user t)
                (:root "~root")))

(test file-path.expand-user.5
  (is-file-path (parse-file-path "~root/a" :expand-user nil)
                ("~root" "a")))

(test file-path.expand-user.6
  (is-file-path (parse-file-path "~root/a" :expand-user t)
                (:root "root" "a")))


(test file-path.namestring.1
  (is (equal "/" (file-path-namestring (file-path "/")))))

(test file-path.namestring.2
  (is (equal "/." (file-path-namestring (file-path "/.")))))

(test file-path.namestring.3
  (is (equal "/.." (file-path-namestring (file-path "/..")))))

(test file-path.namestring.4
  (is (equal "." (file-path-namestring (file-path ".")))))

(test file-path.namestring.5
  (is (equal "./" (file-path-namestring (file-path "./")))))

(test file-path.namestring.6
  (is (equal "../." (file-path-namestring (file-path "../.")))))

(test file-path.namestring.7
  (is (equal ".././" (file-path-namestring (file-path ".././")))))

(test file-path.namestring.8
  (is (equal "../.." (file-path-namestring (file-path "../..")))))

(test file-path.namestring.9
  (is (equal "a/./b" (file-path-namestring (file-path "a/./b")))))

(test file-path.namestring.10
  (is (equal "a/../b" (file-path-namestring (file-path "a/../b")))))


(test file-path.directory.1
  (is (equal '(:root) (file-path-directory (file-path "/")))))

(test file-path.directory.2
  (is (equal '(:root) (file-path-directory (file-path "/.")))))

(test file-path.directory.3
  (is (equal '(:root) (file-path-directory (file-path "/..")))))

(test file-path.directory.4
  (is (equal '(".") (file-path-directory (file-path ".")))))

(test file-path.directory.5
  (is (equal '(".") (file-path-directory (file-path "./")))))

(test file-path.directory.6
  (is (equal '(".") (file-path-directory (file-path "..")))))

(test file-path.directory.7
  (is (equal '(".") (file-path-directory (file-path "../")))))

(test file-path.directory.8
  (is (equal '("..") (file-path-directory (file-path "../.")))))

(test file-path.directory.9
  (is (equal '("..") (file-path-directory (file-path ".././")))))

(test file-path.directory.10
  (is (equal '("..") (file-path-directory (file-path "../..")))))

(test file-path.directory.11
  (is (equal '("..") (file-path-directory (file-path "../../")))))

(test file-path.directory.12
  (is (equal '("a" ".") (file-path-directory (file-path "a/./b")))))

(test file-path.directory.13
  (is (equal '("a" "..") (file-path-directory (file-path "a/../b")))))


(test file-path.directory-namestring.1
  (is (equal "/" (file-path-directory (file-path "/") :namestring t))))

(test file-path.directory-namestring.2
  (is (equal "/" (file-path-directory (file-path "/.") :namestring t))))

(test file-path.directory-namestring.3
  (is (equal "/" (file-path-directory (file-path "/..") :namestring t))))

(test file-path.directory-namestring.4
  (is (equal "./" (file-path-directory (file-path ".") :namestring t))))

(test file-path.directory-namestring.5
  (is (equal "./" (file-path-directory (file-path "./") :namestring t))))

(test file-path.directory-namestring.6
  (is (equal "./" (file-path-directory (file-path "..") :namestring t))))

(test file-path.directory-namestring.7
  (is (equal "./" (file-path-directory (file-path "../") :namestring t))))

(test file-path.directory-namestring.8
  (is (equal "../" (file-path-directory (file-path "../.") :namestring t))))

(test file-path.directory-namestring.9
  (is (equal "../" (file-path-directory (file-path ".././") :namestring t))))

(test file-path.directory-namestring.10
  (is (equal "../" (file-path-directory (file-path "../..") :namestring t))))

(test file-path.directory-namestring.11
  (is (equal "../" (file-path-directory (file-path "../../") :namestring t))))

(test file-path.directory-namestring.12
  (is (equal "a/./" (file-path-directory (file-path "a/./b") :namestring t))))

(test file-path.directory-namestring.13
  (is (equal "a/../" (file-path-directory (file-path "a/../b") :namestring t))))


(test file-path.file.1
  (is (equal "." (file-path-file (file-path "/")))))

(test file-path.file.2
  (is (equal "." (file-path-file (file-path "/.")))))

(test file-path.file.3
  (is (equal ".." (file-path-file (file-path "/..")))))

(test file-path.file.4
  (is (equal "." (file-path-file (file-path ".")))))

(test file-path.file.5
  (is (equal "." (file-path-file (file-path "./")))))

(test file-path.file.6
  (is (equal ".." (file-path-file (file-path "..")))))

(test file-path.file.7
  (is (equal ".." (file-path-file (file-path "../")))))

(test file-path.file.8
  (is (equal "." (file-path-file (file-path "../.")))))

(test file-path.file.9
  (is (equal "." (file-path-file (file-path ".././")))))

(test file-path.file.10
  (is (equal ".." (file-path-file (file-path "../..")))))

(test file-path.file.11
  (is (equal ".." (file-path-file (file-path "../../")))))

(test file-path.file.12
  (is (equal "b" (file-path-file (file-path "a/./b")))))

(test file-path.file.13
  (is (equal "b" (file-path-file (file-path "a/../b")))))


(test file-path.file-name.1
  (is (equal "." (file-path-file-name (file-path "/")))))

(test file-path.file-name.2
  (is (equal "." (file-path-file-name (file-path ".")))))

(test file-path.file-name.3
  (is (equal "." (file-path-file-name (file-path "./")))))

(test file-path.file-name.4
  (is (equal "." (file-path-file-name (file-path "..")))))

(test file-path.file-name.5
  (is (equal "." (file-path-file-name (file-path "../")))))

(test file-path.file-name.6
  (is (equal "a" (file-path-file-name (file-path "a")))))

(test file-path.file-name.7
  (is (equal "a" (file-path-file-name (file-path "a.")))))

(test file-path.file-name.8
  (is (equal ".a" (file-path-file-name (file-path ".a")))))

(test file-path.file-name.9
  (is (equal "a" (file-path-file-name (file-path "a.b")))))


(test file-path.file-type.1
  (is (eql nil (file-path-file-type (file-path "/")))))

(test file-path.file-type.2
  (is (eql nil (file-path-file-type (file-path ".")))))

(test file-path.file-type.3
  (is (eql nil (file-path-file-type (file-path "./")))))

(test file-path.file-type.4
  (is (eql nil (file-path-file-type (file-path "..")))))

(test file-path.file-type.5
  (is (eql nil (file-path-file-type (file-path "../")))))

(test file-path.file-type.6
  (is (eql nil (file-path-file-type (file-path "a")))))

(test file-path.file-type.7
  (is (eql nil (file-path-file-type (file-path "a.")))))

(test file-path.file-type.8
  (is (eql nil (file-path-file-type (file-path ".a")))))

(test file-path.file-type.9
  (is (equal "b" (file-path-file-type (file-path "a.b")))))
