;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;; and this code is released under the same license as IOLib.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base))

(in-package :iolib.asdf)

(defsystem :iolib.examples
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :depends-on (:iolib :bordeaux-threads)
  :default-component-class iolib-source-file
  :components ((:file "package")
               (:file "ex1-client" :depends-on ("package"))
               (:file "ex2-client" :depends-on ("package"))
               (:file "ex3-client" :depends-on ("package"))
               (:file "ex4-client" :depends-on ("package"))
               (:file "ex5a-client" :depends-on ("package"))
               (:file "ex5b-client" :depends-on ("package"))

               (:file "ex1-server" :depends-on ("package"))
               (:file "ex2-server" :depends-on ("package"))
               (:file "ex3-server" :depends-on ("package"))
               (:file "ex4-server" :depends-on ("package"))
               (:file "ex5-server" :depends-on ("package"))
               (:file "ex6-server" :depends-on ("package"))
               (:file "ex7-buffer" :depends-on ("package"))
               (:file "ex7-server" :depends-on ("package" "ex7-buffer"))
               (:file "ex8-buffer" :depends-on ("package"))
               (:file "ex8-server" :depends-on ("package" "ex8-buffer"))))




