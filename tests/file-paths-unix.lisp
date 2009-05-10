;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.pathnames test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.pathnames)

(defmacro is-file-path (path (directory name))
  (with-gensyms (p)
    `(is-true
      (let ((,p ,path))
        (and (eql (file-path-host ,p) :unspecific)
             (eql (file-path-device ,p) :unspecific)
             (equal (file-path-directory ,p) ',directory)
             (equal (file-path-name ,p) ',name))))))

(test parse-file-path.1
  (is-file-path (parse-file-path "")
                ((:relative) nil)))

(test parse-file-path.2
  (is-file-path (parse-file-path "" :as-directory t)
                ((:relative) nil)))

(test parse-file-path.3
  (is-file-path (parse-file-path "" :expand-user t)
                ((:relative) nil)))

(test parse-file-path.4
  (is-file-path (parse-file-path "" :as-directory t :expand-user t)
                ((:relative) nil)))

(test parse-file-path.5
  (is-file-path (parse-file-path "a")
                ((:relative) "a")))

(test parse-file-path.6
  (is-file-path (parse-file-path "a" :as-directory t)
                ((:relative "a") nil)))

(test parse-file-path.7
  (is-file-path (parse-file-path "a" :expand-user t)
                ((:relative) "a")))

(test parse-file-path.8
  (is-file-path (parse-file-path "a" :as-directory t :expand-user t)
                ((:relative "a") nil)))

(test parse-file-path.9
  (is-file-path (parse-file-path "a/")
                ((:relative "a") nil)))

(test parse-file-path.10
  (is-file-path (parse-file-path "a/" :as-directory t)
                ((:relative "a") nil)))

(test parse-file-path.11
  (is-file-path (parse-file-path "a/" :expand-user t)
                ((:relative "a") nil)))

(test parse-file-path.12
  (is-file-path (parse-file-path "a/" :as-directory t :expand-user t)
                ((:relative "a") nil)))

(test parse-file-path.13
  (is-file-path (parse-file-path "/a")
                ((:absolute) "a")))

(test parse-file-path.14
  (is-file-path (parse-file-path "/a" :as-directory t)
                ((:absolute "a") nil)))

(test parse-file-path.15
  (is-file-path (parse-file-path "/a" :expand-user t)
                ((:absolute) "a")))

(test parse-file-path.16
  (is-file-path (parse-file-path "/a" :as-directory t :expand-user t)
                ((:absolute "a") nil)))

(test parse-file-path.17
  (is-file-path (parse-file-path "/a/")
                ((:absolute "a") nil)))

(test parse-file-path.18
  (is-file-path (parse-file-path "/a/" :as-directory t)
                ((:absolute "a") nil)))

(test parse-file-path.19
  (is-file-path (parse-file-path "/a/" :expand-user t)
                ((:absolute "a") nil)))

(test parse-file-path.20
  (is-file-path (parse-file-path "/a/" :as-directory t :expand-user t)
                ((:absolute "a") nil)))

(test parse-file-path.expand-user.1
  (is-file-path (parse-file-path "~root")
                ((:relative) "~root")))

(test parse-file-path.expand-user.2
  (is-file-path (parse-file-path "~root" :as-directory t)
                ((:relative "~root") nil)))

(test parse-file-path.expand-user.3
  (is-file-path (parse-file-path "~root" :expand-user t)
                ((:absolute "root") nil)))

(test parse-file-path.expand-user.4
  (is-file-path (parse-file-path "~root" :as-directory t :expand-user t)
                ((:absolute "root") nil)))

(test parse-file-path.expand-user.5
  (is-file-path (parse-file-path "/~root")
                ((:absolute) "~root")))

(test parse-file-path.expand-user.6
  (is-file-path (parse-file-path "/~root" :as-directory t)
                ((:absolute "~root") nil)))

(test parse-file-path.expand-user.7
  (is-file-path (parse-file-path "/~root" :expand-user t)
                ((:absolute) "~root")))

(test parse-file-path.expand-user.8
  (is-file-path (parse-file-path "/~root" :as-directory t :expand-user t)
                ((:absolute "~root") nil)))
