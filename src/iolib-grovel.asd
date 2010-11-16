;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; iolib-grovel.asd --- ASDF system definition for iolib-grovel.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(asdf:defsystem :iolib-grovel
  :description "The CFFI Groveller"
  :author "Dan Knapp <dankna@accela.net>"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:alexandria :cffi :iolib.asdf :iolib.conf)
  :default-component-class :iolib-source-file
  :licence "MIT"
  :pathname "grovel/"
  :components
  ((:file "package")
   (:file "invoke")
   (:static-file "common.h")
   (:file "grovel")
   (:file "asdf"))
  :serial t
  :perform (compile-op :before (o c)
             #+allegro (require "osi"))
  :perform (load-op :before (o c)
             #+allegro (require "osi")))

;; vim: ft=lisp et
