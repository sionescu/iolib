;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; pkgdcl.lisp --- Package definition.
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

(in-package :sockets/cc-tests)

(in-suite test)

(defparameter *external-format* (babel:ensure-external-format '(:utf-8 :eol-style :crlf)))

(defun start-reverser-server (&key (address +loopback+) (port 4242) (worker-count 4)
                              (external-format *external-format*))
  (bind ((acceptor (make-connection-acceptor 'reverser-connection-handler
                                             :worker-count worker-count
                                             :external-format external-format)))
    (finishes
      (unwind-protect
           (progn
             (startup-acceptor acceptor :address address :port port)
             ;; TODO until epoll based multiplexing is added
             (break "Server started, continue to shut it down")
             ;;(sockets/cc::busy-loop-hack acceptor)
             )
        (shutdown-acceptor acceptor))))
  (values))

(defun/cc reverser-connection-handler (connection)
  (loop
     for line = (read-line/cc connection)
     until (zerop (length line)) do
     (progn
       (write-string (nreverse line) connection)
       (terpri connection)
       (force-output connection))))

#+nil
(defun start-reverser-client (&key (address +loopback+) (port 4242) (worker-count 4)
                              (external-format *external-format*))
  (bind ((multiplexer (make-instance 'connection-multiplexer)))
    )

  (bind ((acceptor (make-connection-acceptor 'reversering-connection-handler
                                             :worker-count worker-count
                                             :external-format '(:utf-8 :eol-style :crlf))))
    (finishes
      (unwind-protect
           (progn
             (startup-acceptor acceptor :address address :port port)
             ;;(break "Acceptor running, continue this thread to shut it down")
             )
        (shutdown-acceptor acceptor))))
  (values))

#+nil
(defun reverser-client-worker-loop (address port external-format)
  (bind ((socket (make-socket :connect :active :external-format external-format))
         (done nil))
    (unwind-protect
         (progn
           (setf (fd-non-blocking socket) t)
           (bind-address socket (ensure-hostname address)
                         :port port
                         :reuse-address t)
           (apply 'listen-on socket (when backlog
                                      (list :backlog backlog)))
           (setf (socket-of acceptor) socket)
           #+nil(loop repeat (worker-count-of acceptor) do
                     (spawn-worker-thread-for-acceptor acceptor))
           (setf done t))
      (unless done
        (close socket)))))
