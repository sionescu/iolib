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

(in-package :net.sockets.cc)

(defun make-connection (fd &key external-format input-buffer-size
                        output-buffer-size continuation wait-reason)
  (bind ((result (make-instance 'connection
                                :file-descriptor fd
                                :continuation continuation
                                :wait-reason wait-reason
                                :external-format external-format
                                :input-buffer-size input-buffer-size
                                :output-buffer-size output-buffer-size)))
    (assert (fd-of result))
    result))

(defun make-client-connection (address &key (port 4242) (external-format :default)
                               continuation wait-reason)
  (bind ((socket (make-socket :connect :active :external-format external-format)))
    (sockets:connect socket address :port port)
    (bind ((connection (make-connection (fd-of socket)
                                        :external-format external-format
                                        :continuation continuation
                                        :wait-reason wait-reason)))
      connection)))

;; TODO test these and hook them up on the stream primitives
(defmethod notify-bytes-read ((connection bandwidth-information-mixin) count)
  (incf (total-bytes-read-of connection) count))

(defmethod average-reading-speed-of ((connection bandwidth-information-mixin))
  (/ (total-bytes-read-of connection)
     (* (- (get-internal-real-time)
           (created-at-of connection))
        internal-time-units-per-second)))

(defmethod notify-bytes-written ((connection bandwidth-information-mixin) count)
  (incf (total-bytes-written-of connection) count))

(defmethod average-writing-speed-of ((connection bandwidth-information-mixin))
  (/ (total-bytes-written-of connection)
     (* (- (get-internal-real-time)
           (created-at-of connection))
        internal-time-units-per-second)))