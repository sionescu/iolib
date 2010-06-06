;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib-tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "MIT"
  :depends-on (:fiveam :iolib :iolib.pathnames)
  :default-component-class iolib.base:cl-source-file
  :components
  ((:file "pkgdcl")
   (:file "defsuites" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "defsuites"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "defsuites")
     :pathname #+unix "file-paths-unix")
   (:file "events" :depends-on ("pkgdcl" "defsuites"))
   (:file "streams" :depends-on ("pkgdcl" "defsuites"))
   (:file "sockets" :depends-on ("pkgdcl" "defsuites"))))

(defmethod perform ((o test-op)
                    (c (eql (find-system :iolib-tests))))
  (operate 'load-op :iolib-tests)
  (funcall (intern (symbol-name '#:run!) '#:5am) :iolib))

(defmethod operation-done-p ((o test-op)
                             (c (eql (find-system :iolib-tests))))
  nil)
