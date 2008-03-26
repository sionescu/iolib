;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: NIL -*-
;;;
;;; Copyright (C) 2006-2008, Attila Lendvai  <attila.lendvai@gmail.com>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :common-lisp-user)

(defpackage :net.sockets.cc
  (:use #:common-lisp :cl-cont :alexandria :io.streams :metabang-bind
        :net.sockets :trivial-gray-streams :io.multiplex)
  (:export

   #:shadowing-import-sockets/cc-symbols

   #:connection
   #:continuation-of
   #:make-connection
   #:make-client-connection
   #:connection-acceptor
   #:make-connection-acceptor

   #:startup-acceptor
   #:shutdown-acceptor

   #:connection-multiplexer
   #:make-connection-multiplexer
   #:register-connection
   #:unregister-connection
   #:startup-connection-multiplexer
   #:shutdown-connection-multiplexer

   ;; primitives that can be used in cc code and transparently putting the connection on sleep
   #:read-char/cc
   #:write-char/cc
   #:read-line/cc
   #:write-line/cc
   #:write-string/cc
   #:wait-until-fd-ready/cc
   )

   ;; import some "internals", although socket/cc is more like an extension then a user lib
  (:shadowing-import-from :net.sockets
   #:with-sockaddr-storage
   #:with-socklen
   #:size-of-sockaddr-storage
   #:%accept
   #:%sendto
   )

  (:shadowing-import-from :io.multiplex
   #:harvest-events
   #:monitor-fd
   #:unmonitor-fd
   #:make-fd-entry
   ))

#|

(defpackage :net.sockets.cc.shadows
  (:nicknames #:sockets/cc)
  (:use #:common-lisp :cl-cont :alexandria :io.streams :metabang-bind :net.sockets :net.sockets.cc)

  (:shadow
   #:read-char
   #:read-line

   #:receive-from
   #:send-to
   )

  (:export
   #:read-char
   #:read-line
   #:read-sequence

   #:write-char
   #:write-string
   #:write-line
   #:write-sequence

   ))

(in-package :net.sockets.cc)

(defun shadowing-import-sockets/cc-symbols (&optional (into-package *package*))
  (bind ((exported-symbols (list)))
    (do-external-symbols (symbol :net.sockets.cc.shadows)
      (push symbol exported-symbols))
    (shadowing-import exported-symbols into-package)))

|#
