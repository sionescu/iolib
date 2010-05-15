;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

(asdf:defsystem :iolib.examples
  :depends-on (:iolib :bordeaux-threads)
  :components ( (:file "package")
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
                (:file "ex8-server" :depends-on ("package" "ex8-buffer"))

                ))




