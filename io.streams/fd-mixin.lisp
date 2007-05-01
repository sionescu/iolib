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

(in-package :io.streams)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  get and set O_NONBLOCK  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %get-fd-nonblock-mode (fd)
  (let ((current-flags (et:fcntl fd et:f-getfl)))
    (logtest et:o-nonblock current-flags)))

(defun %set-fd-nonblock-mode (fd mode)
  (let* ((current-flags (et:fcntl fd et:f-getfl))
         (new-flags (if mode
                        (logior current-flags et:o-nonblock)
                        (logandc2 current-flags et:o-nonblock))))
    (when (/= new-flags current-flags)
      (et:fcntl fd et:f-setfl new-flags))
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
