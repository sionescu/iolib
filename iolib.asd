;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; iolib.asd --- ASDF system definition.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.5.4"
  :licence "LLGPL-2.1"
  :depends-on (:net.sockets
               ;; comment out until it's brought back up to date
               ;; :net.dns-client
               ))

(defmethod perform ((o test-op) (c (eql (find-system :iolib))))
  (operate 'test-op :iolib-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :iolib))))
  nil)
