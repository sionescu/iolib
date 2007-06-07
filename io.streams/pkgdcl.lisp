;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :common-lisp-user)

#-(or sbcl cmu openmcl clisp allegro lispworks)
(error "Your CL implementation is not suported.")

(macrolet ((def-my-package (&rest methods)
             `(defpackage :io.streams
                (:use #:common-lisp #:cffi #:iolib-utils
                      #:io.encodings)
                #+(or sbcl cmu openmcl clisp allegro lispworks)
                (:import-from #+sbcl      #:sb-gray
                              #+cmu       #:ext
                              #+openmcl   #:ccl
                              #+clisp     #:gray
                              #+allegro   #:excl
                              #+lispworks #:stream
                              ,@methods)
                (:export ,@methods
                         #:ub8 #:ub16 #:ub32 #:sb8 #:sb16 #:sb32
                         #:ub8-sarray #:ub8-vector #:ub16-sarray
                         #:external-format-of
                         #:dual-channel-fd-mixin
                         #:input-fd #:input-fd-of #:input-fd-non-blocking
                         #:output-fd #:output-fd-of #:output-fd-non-blocking
                         #:dual-channel-single-fd-mixin
                         #:fd-of #:fd-non-blocking
                         #:dual-channel-gray-stream))))
  (def-my-package
    #:fundamental-binary-input-stream #:fundamental-binary-output-stream
    #:fundamental-character-input-stream #:fundamental-character-output-stream
    #:stream-clear-input #:stream-clear-output
    #:stream-finish-output #:stream-force-output
    #:stream-read-byte #:stream-write-byte
    #:stream-peek-char #:stream-read-char #:stream-read-char-no-hang
    #:stream-unread-char #:stream-read-line #:stream-listen
    #-(or clisp openmcl) #:stream-read-sequence
    #+openmcl #:stream-read-vector
    #+clisp #:stream-read-char-sequence #+clisp #:stream-read-byte-sequence
    #-(or clisp openmcl) #:stream-write-sequence
    #+openmcl #:stream-write-vector
    #+clisp #:stream-write-char-sequence #+clisp #:stream-write-byte-sequence
    #:stream-fresh-line #:stream-start-line-p #:stream-line-column
    #:stream-terpri #:stream-write-char #:stream-write-string))
