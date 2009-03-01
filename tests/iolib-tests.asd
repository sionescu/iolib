;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib-tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "MIT"
  :depends-on (:fiveam :iolib :iolib.pathnames)
  :components
  ((:file "pkgdcl")
   (:file "mainsuite" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "mainsuite"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "mainsuite")
          :pathname #+unix "file-paths-unix")
   (:file "events" :depends-on ("pkgdcl" "mainsuite"))
   (:file "streams" :depends-on ("pkgdcl" "mainsuite"))
   (:file "sockets" :depends-on ("pkgdcl" "mainsuite"))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :iolib-tests))))
  (asdf:operate 'asdf:load-op :iolib-tests)
  (funcall (intern (symbol-name '#:run!) '#:5am) :iolib))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system :iolib-tests))))
  nil)