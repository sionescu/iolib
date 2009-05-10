;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib-tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "MIT"
  :depends-on (:fiveam :iolib :iolib.pathnames)
  :components
  ((:file "pkgdcl")
   (:file "defsuites" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "defsuites"))
   (:file "runes" :depends-on ("pkgdcl" "defsuites"))
   (:file "rods" :depends-on ("pkgdcl" "defsuites"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "defsuites")
          :pathname #+unix "file-paths-unix")
   (:file "events" :depends-on ("pkgdcl" "defsuites"))
   (:file "streams" :depends-on ("pkgdcl" "defsuites"))
   (:file "sockets" :depends-on ("pkgdcl" "defsuites"))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :iolib-tests))))
  (asdf:operate 'asdf:load-op :iolib-tests)
  (funcall (intern (symbol-name '#:run!) '#:5am) :iolib))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system :iolib-tests))))
  nil)
