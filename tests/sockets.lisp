;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; sockets.lisp --- net.sockets test suite.
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
;;; The first version of this test suite was based on SB-BSD-SOCKETS'
;;; to which the following licensing information applies:
;;;
;;;    All changes to SBCL since the fork from CMU CL have been
;;;    released into the public domain in jurisdictions where this is
;;;    possible, or under the FreeBSD licence where not.

(in-package #:iolib-tests)

;;; A couple of these tests require an echo server.  You can either
;;; compile and run the provided tests/echo-server.c or enabled the
;;; echo services in (x)inetd.
;;;
;;; (Note: on Darwin, this can be achieved by uncommenting the echo
;;;  service in /etc/inetd.conf and running:
;;;    sudo xinetd -dontfork -inetd_compat)
;;;
;;; Set these appropriately if you want to point the echo tests
;;; somewhere else.
(defparameter *echo-address* (ensure-address #(127 0 0 1)))
(defparameter *echo-port* 7)

;;; Returns T if one of the expected conditions occured, otherwise returns
;;; a list of the form (:RESULT return-value-1 return-value-2) with
;;; the return values from BODY.
(defmacro with-expected-conditions ((&rest conditions) &body body)
  `(handler-case (progn ,@body)
    ,@(loop for c in conditions collect `(,c () t))
    (:no-error (&rest result) (list* :result result))))

;;;; Addresses

;;; a real address
(deftest address-to-vector.1
    (address-to-vector "127.0.0.1")
  #(127 0 0 1) :ipv4)

;;; and an address with bit 8 set on some octets
(deftest address-to-vector.2
    (address-to-vector "242.1.211.3")
  #(242 1 211 3) :ipv4)

(deftest address-to-vector.3
    (address-to-vector "::")
  #(0 0 0 0 0 0 0 0) :ipv6)

;;; RT: used to return the PARSE-ERROR as a secondary value.
(deftest string-address-to-vector.1
    (string-address-to-vector "256.0.0.1")
  nil)

;;; RT: should only ignore PARSE-ERRORs.
(deftest string-address-to-vector.2
    (handler-case (string-address-to-vector 'not-a-string)
      (type-error () t))
  t)

;;; RT: should signal a PARSE-ERROR when given an invalid string.
(deftest ensure-address.1
    (handler-case (ensure-address "ff0x::114")
      (parse-error () t))
  t)

;;; ditto
(deftest ensure-address.2
    (handler-case (ensure-address "127.0.256.1")
      (parse-error () t))
  t)

(deftest integer-to-dotted-and-back
    (loop for string in '("0.0.255.0" "0.255.255.0" "0.255.255.1")
          always (string= string
                          (integer-to-dotted (dotted-to-integer string))))
  t)

(deftest integer-to-dotted.1
    (values (integer-to-dotted 0) (integer-to-dotted +max-ipv4-value+))
  "0.0.0.0" "255.255.255.255")

(deftest integer-to-dotted.2
    (values (handler-case (integer-to-dotted (1+ +max-ipv4-value+))
              (type-error () t))
            (handler-case (integer-to-dotted -1)
              (type-error () t)))
  t t)

(deftest dotted-to-vector.1
    (mapcar #'dotted-to-vector '("255.255.255.255" "0.0.0.0" "127.0.0.1"))
  (#(255 255 255 255) #(0 0 0 0) #(127 0 0 1)))

(deftest dotted-to-vector.2
    (handler-case (dotted-to-vector "127.0.0.0.0")
      (parse-error () t))
  t)

(deftest dotted-to-vector.3
    (handler-case (dotted-to-vector 'not-a-string)
      (type-error () t))
  t)

(deftest vector-to-dotted.1
    (mapcar #'vector-to-dotted '(#(255 255 255 255) #(0 0 0 0) (127 0 0 1)))
  ("255.255.255.255" "0.0.0.0" "127.0.0.1"))

(deftest vector-to-dotted.2
    (handler-case (vector-to-dotted #(127 0 0 256))
      (type-error () t))
  t)

(deftest address-to-string.1
    (mapcar (lambda (x) (address-to-string (make-address x)))
            '(#(127 0 0 1) #(255 255 255 255) #(0 0 0 0 0 0 0 0)
              #(0 0 0 0 0 0 0 1) #(1 0 0 0 0 0 0 0)))
  ("127.0.0.1" "255.255.255.255" "::" "::1" "1::"))

(deftest vector-to-colon-separated.1
    (let ((ip  #(0 0 0 255 255 255 0 0)))
      (values (vector-to-colon-separated ip)
              (vector-to-colon-separated ip :downcase)
              (vector-to-colon-separated ip :upcase)))
  "::ff:ff:ff:0:" "::ff:ff:ff:0:" "::FF:FF:FF:0:")

(deftest vector-to-colon-separated.2
    (vector-to-colon-separated #(1 2 3 4 5 0 6 7))
  "1:2:3:4:5::6:7")

(deftest vector-to-colon-separated.3
    (vector-to-colon-separated #(0 2 3 4 5 0 6 7))
  ":2:3:4:5::6:7")

(deftest vector-to-colon-separated.4
    (vector-to-colon-separated #(1 2 3 4 5 0 6 0))
  "1:2:3:4:5::6:")

(deftest colon-separated-to-vector.1
    (mapcar #'colon-separated-to-vector
            '(":ff::ff:" "::" "::1" "1::" ":2:3:4:5:6:7:8" "1:2:3:4:5:6:7:"
              ":1::2:" "::127.0.0.1" ":1::127.0.0.1"))
  (#(0 #xff 0 0 0 0 #xff 0)
    #(0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 1)
    #(1 0 0 0 0 0 0 0)
    #(0 2 3 4 5 6 7 8)
    #(1 2 3 4 5 6 7 0)
    #(0 1 0 0 0 0 2 0)
    #(0 0 0 0 0 0 #x7f00 1)
    #(0 1 0 0 0 0 #x7f00 1)))

(deftest address=.1
    (address= +ipv4-loopback+ (make-address #(127 0 0 1)))
  t)

(deftest address=.2
    (address= +ipv6-loopback+ (ensure-address "::1"))
  t)

(deftest copy-address.1
    (loop for designator in (list "127.0.0.1" +max-ipv4-value+ "::" "::1")
          for addr1 = (ensure-address designator)
          for addr2 = (ensure-address designator)
          for addr3 = (copy-address addr1)
          always (and (address= addr1 addr2)
                      (address= addr1 addr3)
                      (address= addr2 addr3)))
  t)

(deftest make-address.1
    (handler-case (make-address 'not-a-valid-designator)
      (type-error () t))
  t)

(deftest address.unspecified.1
    (every #'inet-address-unspecified-p
           (mapcar #'ensure-address '("0.0.0.0" "::" "0:0:0:0:0:0:0:0")))
  t)

(deftest address.loopback.1
    (every #'inet-address-loopback-p
           (mapcar #'ensure-address '("127.0.0.1" "::1" "0:0::1")))
  t)

(deftest address.multicast.1
    (every #'inet-address-multicast-p
           (mapcar #'ensure-address
                   '("224.0.0.0" "224.1.2.3" "232.0.0.127" "239.255.255.255"
                     "ff02::2" "ff0a::114" "ff05::1:3")))
  t)

(deftest address.ipv6-ipv4-mapped.1
    (ipv6-ipv4-mapped-p (ensure-address "::ffff:127.0.0.1"))
  t)

(deftest address.ipv6.1
    (address-to-vector "::1.2.3.4")
  #(0 0 0 0 0 0 #x0102 #x0304)
  :ipv6)

;;;; Host Lookup

#-no-internet-available
(deftest lookup-host.1
    (multiple-value-bind (addresses truename)
        (lookup-host "a.root-servers.net" :ipv6 nil)
      (values (address-equal-p (car addresses) #(198 41 0 4))
              truename))
  t "a.root-servers.net")

#-no-internet-available
(deftest lookup-host.2
    (nth-value 1 (lookup-host #(198 41 0 4)))
  "a.root-servers.net")

;;; These days lots of people seem to be using DNS servers that don't
;;; report resolving failures for non-existing domains.  This test
;;; will fail there.
(deftest lookup-host.3
    (with-expected-conditions (resolver-no-name-error)
      (lookup-host "foo.tninkpad.telent.net."))
  t)

(deftest lookup-host.4
    (address-equal-p (car (lookup-host #(127 0 0 1) :ipv6 nil))
                     #(127 0 0 1))
  t)

(deftest lookup-host.5
    (with-expected-conditions (parse-error)
      (lookup-host #(127 0 0)))
  t)

(deftest lookup-host.6
    (with-expected-conditions (resolver-no-name-error)
      (lookup-host #(127 0 0 1) :ipv6 :ipv6))
  t)

(deftest make-host.1
    (listp (host-addresses (make-host "foo" (make-address #(127 0 0 1)))))
  t)

(deftest host-random-address.1
    (address-equal-p (host-random-address
                      (make-host "foo" (make-address #(127 0 0 1))))
                     #(127 0 0 1))
  t)

;;;; Service Lookup

(deftest lookup-service.1
    (lookup-service "ssh")
  22 "ssh" :tcp)

(deftest lookup-service.2
    (lookup-service 22 :udp)
  22 "ssh" :udp)

;;; looks up a reserved service port
(deftest lookup-service.3
    ;; TODO: check for a more specific error.
    (with-expected-conditions (unknown-service)
      (lookup-service 1024))
  t)

;;;; Protocol Lookup

(deftest lookup-protocol.1
    (multiple-value-bind (number name)
        (lookup-protocol "tcp")
      (values number name))
  6 "tcp")

(deftest lookup-protocol.2
    (multiple-value-bind (number name)
        (lookup-protocol "udp")
      (values number name))
  17 "udp")

(deftest lookup-protocol.3
    (with-expected-conditions (unknown-protocol)
      (lookup-protocol "nonexistent-protocol"))
  t)

(deftest lookup-protocol.4
    (nth-value 1 (lookup-protocol 6))
  "tcp")

;;;; Network Interfaces

(deftest list-network-interfaces.1
    (<= 1 (length (list-network-interfaces)))
  t)

(deftest network-interfaces.1
    (flet ((nif-equal (if1 if2)
             (check-type if1 interface)
             (check-type if2 interface)
             (and (string= (interface-name if1) (interface-name if2))
                  (eql (interface-index if1) (interface-index if2)))))
      (loop for nif in (list-network-interfaces)
            always (nif-equal nif (lookup-interface (interface-name nif)))
            always (nif-equal nif (lookup-interface (interface-index nif)))))
  t)

;;;; Sockets

;;; RT: don't accept unknown keyword arguments, such as typos.
(deftest make-socket.1
    (with-expected-conditions (error)
      (make-socket :this-kw-arg-doesnt-exist t))
  t)

(deftest make-socket.2
    (with-socket (s :family :ipv4)
      (values (socket-connected-p s)
              (socket-open-p s)
              (> (socket-fd s) 1)
              (socket-family s)
              (socket-protocol s)))
  nil t t :ipv4 :default) ; why isn't it :TCP?

(deftest make-socket.3
    (with-socket (s :family :ipv4)
      (typep s 'socket))
  t)

;;; Given the functions we've got so far, if you can think of a better
;;; way to make sure the bind succeeded than trying it twice, let me
;;; know. 1974 has no special significance, unless you're the same age
;;; as me.
(deftest inet.socket-bind.1
    (with-socket (s :family :ipv4 :local-host #(127 0 0 1) :local-port 1974)
      (handler-case
          (with-socket (s :family :ipv4 :local-host #(127 0 0 1)
                          :local-port 1974)
            nil)
        (socket-address-in-use-error () t)))
  t)

(deftest sockopt.1
    (with-socket (s :family :ipv4)
      (setf (socket-option s :reuse-address) t)
      (socket-option s :reuse-address))
  t)

;;; Like READ-SEQUENCE, but returns early if the full quantity of data
;;; isn't there to be read.  Blocks if no input at all.
(defun read-buf-nonblock (buffer stream)
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

(deftest simple-tcp-client
    (with-socket (s :remote-host *echo-address* :remote-port *echo-port*
                    :family :ipv4)
      (let ((data (make-string 200)))
        (format s "here is some text")
        (finish-output s)
        (let ((data (subseq data 0 (read-buf-nonblock data s))))
          ;; (format t "~&Got ~S back from TCP echo server~%" data)
          (> (length data) 0))))
  t)

(deftest sockaddr-return-type
    (with-socket (s :remote-host *echo-address* :remote-port *echo-port*
                    :family :ipv4)
      (and (ipv4-address-p (remote-address s))
           (numberp (remote-port s))))
  t)

;;; We don't support streams with UDP sockets ATM.  But when we do,
;;; let's add a similar test using stream functions.
;;;
;;; FIXME: figure out why this test blocks with the inetd services on
;;; my machines, on both Darwin and Linux/x86-64.  Works with
;;; echo-server.c though --luis
(deftest simple-udp-client.1
    (with-socket (s :remote-host *echo-address* :remote-port *echo-port*
                    :type :datagram :family :ipv4)
      (let ((data (make-array '(200) :element-type '(unsigned-byte 8))))
        (socket-send "here is some text" s)
        (socket-receive data s)
        ;; (format t "~&Got ~S back from UDP echo server~%" data)
        (> (length data) 0)))
  t)

(deftest simple-udp-client.2
    (with-socket (s :type :datagram :family :ipv4)
      (let ((data (make-array 100 :element-type '(unsigned-byte 8))))
        (socket-send "here is some more text" s
                     :remote-address *echo-address*
                     :remote-port *echo-port*)
        (socket-receive data s)
        (> (length data) 0)))
  t)

(deftest simple-local-sockets
    (let ((file (namestring
                 (make-pathname
                  :name "local-socket"
                  :type nil
                  :defaults
                  (asdf:system-definition-pathname
                   (asdf:find-system '#:iolib-tests))))))
      ;; (ignore-errors (delete-file file))
      (with-socket (p :family :local :connect :passive :local-filename file)
        (with-socket (a :family :local :remote-filename file)
          (format a "local socket test")
          (finish-output a))
        (let ((s (accept-connection p)))
          (prog1 (read-line s)
            (close s)
            (delete-file file)))))
  "local socket test")

(defmacro with-http-stream ((var host port request) &body body)
  `(with-socket (,var :family :ipv4 :remote-host ,host :remote-port ,port)
     (format ,var ,(concatenate 'string request " HTTP/1.0~%~%"))
     (finish-output ,var)
     ,@body))

#-no-internet-available
(deftest simple-http-client
    (handler-case
        (with-http-stream (s "www.google.com" 80 "HEAD /")
          (let ((data (make-string 200)))
            (setf data (subseq data 0 (read-buf-nonblock data s)))
            ;; (princ data)
            (> (length data) 0)))
      (socket-network-unreachable-error () 'network-unreachable))
  t)

#-no-internet-available
(deftest sockopt-receive-buffer
    ;; on Linux x86, the receive buffer size appears to be doubled in the
    ;; kernel: we set a size of x and then getsockopt() returns 2x.
    ;; This is why we compare with >= instead of =
    (handler-case
        (with-http-stream (s "www.google.com" 80 "HEAD/")
          (setf (socket-option s :receive-buffer) 1975)
          (let ((data (make-string 200)))
            (setf data (subseq data 0 (read-buf-nonblock data s)))
            (and (> (length data) 0)
                 (>= (socket-option s :receive-buffer) 1975))))
      (network-unreachable-error () 'network-unreachable))
  t)

(deftest socket-open-p.1
    (with-socket (s)
      (socket-open-p s))
  t)

(deftest socket-open-p.2
    (with-socket (s :remote-host *echo-address* :remote-port *echo-port*
                    :family :ipv4)
      (socket-open-p s))
  t)

(deftest socket-open-p.3
    (with-socket (s)
      (close s)
      (socket-open-p s))
  nil)

;;; we don't have an automatic test for some of this yet.  There's no
;;; simple way to run servers and have something automatically connect
;;; to them as client, unless we spawn external programs.  Then we
;;; have to start telling people what external programs they should
;;; have installed.  Which, eventually, we will, but not just yet

;;; to check with this: can display packets from multiple peers
;;; peer address is shown correctly for each packet
;;; packet length is correct
;;; long (>500 byte) packets have the full length shown (doesn't work)
#-(and)
(defun udp-server (port)
  (with-socket (s :type :datagram :local-port port)
    (loop
     (multiple-value-bind (buf len address port)
         (socket-receive s nil 500)
       (format t "Received ~A bytes from ~A:~A - ~A ~%"
               len address port (subseq buf 0 (min 10 len)))))))
