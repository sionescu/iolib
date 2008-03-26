;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: NIL -*-
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
  (bind ((acceptor (make-connection-acceptor "reverser server" 'reverser-connection-handler
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
     :for line = (read-line/cc connection nil :eof)
     :until (or (eq line :eof)
                (zerop (length line)))
     :do (progn
           (format *debug-io* "SERVER: read line ~S~%" line)
           (write-string/cc (nreverse line) connection)
           (terpri connection)
           (finish-output connection)
           (format *debug-io* "SERVER: written, looping~%" line))))

(defun start-reverser-client (&key (address +loopback+) (port 4242) (worker-count 0)
                              (external-format *external-format*))
  (if (zerop worker-count)
      (with-open-socket (stream :remote-host address :remote-port port :external-format external-format)
        (loop
           (format *debug-io* "CLIENT: writing line~%")
           (write-line "asdf" stream)
           (finish-output stream)
           (format *debug-io* "CLIENT: reading answer~%")
           (format *debug-io* "CLIENT: at ~A, answer is ~S~%" (get-universal-time) (read-line stream))
           (sleep 1)))
      (error "TODO not yet")))

#+nil(defun start-reverser-client (&key (address +loopback+) (port 4242) ;; (worker-count 4)
                              (external-format *external-format*))
  (bind ((multiplexer (make-connection-multiplexer "reverser client"))
         (connection (make-client-connection address :port port
                                             :external-format external-format
                                             :wait-reason :write)))
    (setf (continuation-of connection) (with-call/cc
                                         (let/cc k
                                           k)
                                         (loop
                                            (write-line/cc "asdf" connection)
                                            (finish-output connection)
                                            (format *debug-io* "~A: ~S" (get-universal-time) (read-line/cc connection)))))
    (startup-connection-multiplexer multiplexer)
    (register-connection multiplexer connection)))

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
