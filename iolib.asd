;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :iolib/asdf
  :description "A few ASDF component classes."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (:alexandria)
  :encoding :utf-8
  :pathname "src/base/"
  :components
  ((:file "asdf")))

(defsystem :iolib/conf
  :description "Compile-time configuration for IOLib."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/conf/"
  :components
  ((:file "pkgdcl")
   (:file "requires" :depends-on ("pkgdcl"))))

(defsystem :iolib/common-lisp
  :description "Slightly modified Common Lisp."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:alexandria)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/new-cl/"
  :components
  ((:file "conduits")
   #+scl (:file "scl-gray-streams")
   (:file "pkgdcl" :depends-on ("conduits" #+scl "scl-gray-streams")
    :perform
    (compile-op :before (o c)
      (symbol-call :iolib/conf '#:load-gray-streams))
    :perform
    (load-op :before (o c)
      (symbol-call :iolib/conf '#:load-gray-streams))
    :perform
    (load-source-op :before (o c)
      (symbol-call :iolib/conf '#:load-gray-streams)))
   (:file "gray-streams"
    :depends-on ("pkgdcl" #+scl "scl-gray-streams"))
   (:file "definitions" :depends-on ("pkgdcl"))
   (:file "types" :depends-on ("pkgdcl"))))

(defsystem :iolib/base
  :description "Base IOlib package, used instead of CL."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/common-lisp :alexandria :split-sequence)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/base/"
  :components
  ((:file "pkgdcl")
   (:file "return-star" :depends-on ("pkgdcl"))
   (:file "types" :depends-on ("pkgdcl" "return-star"))
   (:file "debug" :depends-on ("pkgdcl" "return-star"))
   (:file "conditions" :depends-on ("pkgdcl" "return-star"))
   (:file "defalias" :depends-on ("pkgdcl" "return-star"))
   (:file "deffoldable" :depends-on ("pkgdcl" "return-star"))
   (:file "defobsolete" :depends-on ("pkgdcl" "return-star"))
   (:file "reader" :depends-on ("pkgdcl" "return-star" "conditions"))
   (:file "sequence" :depends-on ("pkgdcl" "return-star"))
   (:file "matching" :depends-on ("pkgdcl" "return-star"))
   (:file "time" :depends-on ("pkgdcl" "return-star"))
   (:file "dynamic-buffer" :depends-on ("pkgdcl" "return-star" "sequence"))))

(defsystem :iolib/grovel
  :description "The CFFI Groveller"
  :author "Dan Knapp <dankna@accela.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/asdf :iolib/base :iolib/conf
               :alexandria :split-sequence #+allegro (:require "osi") :cffi :uiop)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/grovel/"
  :components
  ((:file "package")
   (:static-file "grovel-common.h")
   (:file "grovel")
   (:file "asdf"))
  :serial t)

(defsystem :iolib/syscalls
  :description "Syscalls and foreign types."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf :iolib/grovel)
  :depends-on (:trivial-features :cffi :iolib/base :iolib/grovel)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/syscalls/"
  :components
  ((:file "pkgdcl")
   #+unix
   (:file "syscall-path-strings" :pathname "unix-syscall-path-strings")
   ;; Platform-specific files
   (:iolib-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "conditions")
   (:file "os-conditions" :pathname #+unix "os-conditions-unix")
   (:file "designators")
   (:file "early")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"))
  :serial t)

(defsystem :iolib/multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/base :iolib/syscalls :cffi)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/multiplex/"
  :components
  ((:file "pkgdcl")

   ;; Scheduler
   (:file "utils" :depends-on ("pkgdcl"))
   (:file "timers" :depends-on ("pkgdcl"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))

   ;; Event loop
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "timers" "queue" "scheduler"
                                             "fd-entry" "multiplexer"))

   ;; FD wait
   (:file "fd-wait" :depends-on ("pkgdcl" "utils"))

   ;; Event sources
   (:file "backend-select"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+linux
   (:file "backend-epoll"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+bsd
   (:file "backend-kqueue"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   (:file "detect"
    :depends-on ("pkgdcl" "multiplexer" "backend-select"
                          #+linux "backend-epoll" #+bsd "backend-kqueue"))))

(defsystem :iolib/streams
  :description "Gray streams."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/base :iolib/multiplex :cffi)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/streams/gray/"
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "io-helpers"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"
                  "io-helpers"))))

(defsystem :iolib/zstreams
  :description "Zeta streams."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf)
  :depends-on (:iolib/base :iolib/syscalls :iolib/pathnames :cffi :bordeaux-threads)
  :around-compile "iolib.asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/streams/zeta/"
  :components
  ((:file "pkgdcl")
   (:file "types" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))

   ;; Platform-specific files
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"
     :depends-on ("pkgdcl" "conditions"))

   ;; Device interface definition
   (:file "device" :depends-on ("pkgdcl" "types"))

   ;; Low-level buffers
   (:file "iobuf" :depends-on ("pkgdcl" "types"))

   ;; Streams
   (:file "stream" :depends-on ("pkgdcl" "types" "conditions" "device" "iobuf"))

   ;; Devices
   (:file "file" :pathname #+unix "file-unix"
     :depends-on ("pkgdcl" "types" "conditions" "ffi-functions" "device" "stream"))))

(defsystem :iolib/sockets
  :description "Socket library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf :iolib/grovel)
  :depends-on (:iolib/base :iolib/syscalls :iolib/streams
               :babel :cffi :iolib/grovel :bordeaux-threads
               :idna :swap-bytes)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/sockets/"
  :components
  ((:file "pkgdcl")
   (:iolib-grovel-file "grovel" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "grovel"))
   (:file "bsd" :depends-on ("pkgdcl" "grovel" "conditions"))
   (:file "common" :depends-on ("pkgdcl" "grovel" "bsd"))
   (:file "config" :depends-on ("pkgdcl" "grovel" "bsd"))

   (:file "iface" :depends-on ("pkgdcl" "grovel" "bsd" "common"))

   (:file "address" :depends-on ("pkgdcl" "common"))
   (:file "address-predicates" :depends-on ("pkgdcl" "common" "address"))
   (:file "address-arithmetic" :depends-on ("pkgdcl" "common" "address" "address-predicates"))

   (:file "base-sockets" :depends-on ("pkgdcl" "bsd" "common" "config"))
   (:file "socket-options"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config" "base-sockets"))

   ;; Local file configuration (/etc/hosts etc...)
   (:file "etc-files" :pathname "namedb/etc-files"
     :depends-on ("pkgdcl"))
   (:file "file-monitor" :pathname "namedb/file-monitor"
     :depends-on ("pkgdcl"))
   (:file "protocols" :pathname "namedb/protocols"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "services" :pathname "namedb/services"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "hosts" :pathname "namedb/hosts"
     :depends-on ("pkgdcl" "address" "address-predicates" "etc-files" "file-monitor"))

   (:file "socket-methods"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config"
                  "address" "address-predicates" "base-sockets" "socket-options"
                  "protocols" "services"))
   (:file "make-socket"
     :depends-on ("pkgdcl" "grovel" "common" "config" "address" "address-predicates"
                  "base-sockets" "socket-options" "services" "socket-methods"))

   ;; DNS client
   (:file "dns-common" :pathname "dns/common"
     :depends-on ("pkgdcl" "common"))
   (:file "nameservers" :pathname "dns/nameservers"
     :depends-on ("pkgdcl" "address" "address-predicates" "etc-files" "file-monitor"))
   (:file "message":pathname "dns/message"
     :depends-on ("pkgdcl" "common" "dns-common"))
   (:file "query" :pathname "dns/query"
     :depends-on ("pkgdcl" "conditions" "address" "address-predicates"
                  "socket-options" "socket-methods" "make-socket" "dns-common"
                  "nameservers" "message"))
   (:file "dns-conditions" :pathname "dns/conditions"
     :depends-on ("pkgdcl"))
   (:file "lookup" :pathname "dns/lookup"
     :depends-on ("pkgdcl" "address" "address-predicates" "file-monitor" "hosts"
                  "nameservers" "message" "query" "dns-conditions"))))

(defsystem :iolib/trivial-sockets
  :description "Trivial-Sockets compatibility layer."
  :author "Dan Barlow <dan@telent.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/base :iolib/sockets)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/sockets/"
  :components
  ((:file "trivial-sockets")))

(defsystem :iolib/pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/base :iolib/syscalls)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/pathnames/"
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix"))
  :serial t)

(defsystem :iolib/os
  :description "OS interface."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf :iolib/grovel)
  :depends-on (:iolib/base :iolib/grovel :iolib/syscalls
               :iolib/streams :iolib/pathnames)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/os/"
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix")
   (:iolib-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix")
   (:file "create-process" :pathname #+unix "create-process-unix"))
  :serial t)

(defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/asdf :iolib/conf)
  :depends-on (:iolib/base :iolib/multiplex :iolib/streams :iolib/sockets)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/iolib/"
  :components ((:file "pkgdcl")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :iolib))))
  (load-system :iolib/tests)
  (symbol-call :5am :run! :iolib))

(defsystem :iolib/tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/base)
  :depends-on (:fiveam :iolib :iolib/pathnames)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "tests/"
  :components
  ((:file "pkgdcl")
   (:file "defsuites" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "defsuites"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "defsuites")
     :pathname #+unix "file-paths-unix")
   (:file "events" :depends-on ("pkgdcl" "defsuites"))
   (:file "streams" :depends-on ("pkgdcl" "defsuites"))
   (:file "sockets" :depends-on ("pkgdcl" "defsuites"))))

(defsystem :iolib/examples
  :description "Examples for IOLib tutorial at http://pages.cs.wisc.edu/~psilord/blog/data/iolib-tutorial/tutorial.html"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib/base)
  :depends-on (:iolib :bordeaux-threads)
  :around-compile "iolib/asdf:compile-wrapper"
  :pathname "examples/"
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
