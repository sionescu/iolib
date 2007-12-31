;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; fd-mixin.lisp --- insert description here
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

(in-package :io.streams)

;;;; Get and Set O_NONBLOCK

(defun %get-fd-nonblock-mode (fd)
  (let ((current-flags (nix:fcntl fd nix:f-getfl)))
    (logtest nix:o-nonblock current-flags)))

(defun %set-fd-nonblock-mode (fd mode)
  (let* ((current-flags (nix:fcntl fd nix:f-getfl))
         (new-flags (if mode
                        (logior current-flags nix:o-nonblock)
                        (logandc2 current-flags nix:o-nonblock))))
    (when (/= new-flags current-flags)
      (nix:fcntl fd nix:f-setfl new-flags))
    (values mode)))

(defmethod input-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (%get-fd-nonblock-mode (fd-of fd-mixin)))

(defmethod (setf input-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (fd-of fd-mixin) mode))

(defmethod output-fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (%get-fd-nonblock-mode (output-fd-of fd-mixin)))

(defmethod (setf output-fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (output-fd-of fd-mixin) mode))

(defmethod fd-non-blocking ((fd-mixin dual-channel-single-fd-mixin))
  (%get-fd-nonblock-mode (fd-of fd-mixin)))

(defmethod (setf fd-non-blocking) (mode (fd-mixin dual-channel-single-fd-mixin))
  (check-type mode boolean "a boolean value")
  (%set-fd-nonblock-mode (fd-of fd-mixin) mode))
