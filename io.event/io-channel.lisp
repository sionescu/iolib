;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-channel.lisp - Manage a single I/O channel.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :io.multiplex)

;;;; IO-Channel

;;; FIXME: for the moment channels are read/write
;;;        this will probably have to change

(defclass io-channel ()
     ((protocol :accessor protocol-of)))

(defconstant +default-read-window-size+ 8192)

(defclass io-buffered-channel (io-channel)
     ((read-buffer :accessor read-buffer-of)
      (read-buffered-p :initarg :read-buffered-p
                      :accessor read-buffered-p)
      (read-window-size :initarg :read-window-size
                        :accessor read-window-size-of)
      (write-buffer :accessor write-buffer-of)
      (write-buffered-p :initarg :write-buffered-p
                       :accessor write-buffered-p))
  (:default-initargs
      :read-buffered-p t
      :write-buffered-p t
      :read-window-size +default-read-window-size+))

(defmethod initialize-instance :after ((channel io-buffered-channel) &key
                                       read-buffer-size write-buffer-size)
  (when (read-buffered-p channel)
    (setf (read-buffer-of channel)
          (make-instance 'io-buffer :size read-buffer-size)))
  (when (write-buffered-p channel)
    (setf (write-buffer-of channel)
          (make-instance 'io-buffer :size write-buffer-size))))

;;;; Socket-Transport

(defclass socket-transport (io-buffered-channel)
     ((socket :accessor socket-of)))

;;; FIXME: apply not good
(defmethod initialize-instance ((transport socket-transport)
                                &rest args)
  (apply #'make-socket args))

(defclass tcp-transport (socket-transport)
     ((status :initform :unconnected
              :accessor status-of)))

(defclass udp-transport (socket-transport) ())

(defgeneric read-bytes (transport))

(defgeneric write-bytes (transport bytes &key start end))

(defmethod read-bytes ((c tcp-transport))
  (with-accessors ((rb read-buffer-of)
                   (sock socket-of)
                   (proto protocol-of)
                   (status status-of)) c
    (when (eq status :unconnected)
      (on-connection-ready proto))
    (multiple-value-bind (buf byte-num)
        (handler-case
            ;; append to the buffer
            (socket-receive (data-of rb) sock
                            :start (end-of rb)
                            :end (size-of rb))
          ;; either a spurious event, or the socket has
          ;; just connected and there is no data to receive
          (nix:ewouldblock ()
            (return-from read-bytes))
          ;; FIXME: perhaps we might be a little more sophisticated here
          (socket-error (err)
            (on-connection-lost proto err)))
      (cond
        ;; EOF
        ((zerop byte-num)
         (on-connection-end proto))
        ;; good data
        ((plusp byte-num)
         ;; increment the end pointer of the buffer
         (incf (end-of rb) byte-num)
         ;; FIXME: we're both buffering the data *and* calling
         ;; ON-MESSAGE-RECEIVED with an array displaced to
         ;; the data just received. perhaps we should separate the two
         (on-message-received
          proto
          (make-array byte-num :element-type '(unsigned-byte 8)
                      :displaced-to (data-of rb)
                      :displaced-index-offset )))))))

(defmethod write-bytes ((c tcp-transport)
                        bytes &key start end)
  )
