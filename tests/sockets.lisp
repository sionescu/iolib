;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- net.sockets test suite.
;;;

(in-package :iolib-tests)

(in-suite* :net.sockets :in :iolib)

(defparameter *echo-address* (ensure-address #(127 0 0 1)))
(defparameter *echo-port* 7)

;;;; Addresses

;;; a real address
(test address-to-vector.1
  (is (equalp (address-to-vector "127.0.0.1")
              (values #(127 0 0 1) :ipv4))))

;;; and an address with bit 8 set on some octets
(test address-to-vector.2
  (is (equalp (address-to-vector "242.1.211.3")
              (values #(242 1 211 3) :ipv4))))

(test address-to-vector.3
  (is (equalp (address-to-vector "::")
              (values #(0 0 0 0 0 0 0 0) :ipv6))))

;;; RT: used to return the PARSE-ERROR as a secondary value.
(test string-address-to-vector.1
  (is-false (string-address-to-vector "256.0.0.1")))

;;; RT: should only ignore PARSE-ERRORs.
(test string-address-to-vector.2
  (signals type-error
    (string-address-to-vector 'not-a-string)))

;;; RT: should signal a PARSE-ERROR when given an invalid string.
(test ensure-address.1
  (signals parse-error
    (ensure-address "ff0x::114")))

;;; ditto
(test ensure-address.2
  (signals parse-error
    (ensure-address "127.0.256.1")))

(test ensure-address.3
  (is-false
   (or (ensure-address "ff0x::114" :errorp nil)
       (ensure-address "127.0.256.1" :errorp nil))))

(test integer-to-dotted-and-back
  (is-true
   (every #'(lambda (s) (string= s (integer-to-dotted (dotted-to-integer s))))
          '("0.0.255.0" "0.255.255.0" "0.255.255.1"))))

(test integer-to-dotted.1
  (is (string= (integer-to-dotted 0)
               "0.0.0.0")))

(test integer-to-dotted.2
  (is (string= (integer-to-dotted +max-ipv4-value+)
               "255.255.255.255")))

(test integer-to-dotted.3
  (signals type-error
    (integer-to-dotted (1+ +max-ipv4-value+))))

(test integer-to-dotted.4
  (signals type-error
    (integer-to-dotted -1)))

(test dotted-to-vector.1
  (is (equalp (mapcar #'dotted-to-vector '("255.255.255.255" "0.0.0.0" "127.0.0.1"))
              '(#(255 255 255 255) #(0 0 0 0) #(127 0 0 1)))))

(test dotted-to-vector.2
  (signals parse-error
    (dotted-to-vector "127.0.0.0.0")))

(test dotted-to-vector.3
  (signals type-error
    (dotted-to-vector 'not-a-string)))

(test vector-to-dotted.1
  (is (equalp (mapcar #'vector-to-dotted '(#(255 255 255 255) #(0 0 0 0) (127 0 0 1)))
              '("255.255.255.255" "0.0.0.0" "127.0.0.1"))))

(test vector-to-dotted.2
  (signals type-error
    (vector-to-dotted #(127 0 0 256))))

(test address-to-string.1
  (is (equalp (mapcar (lambda (x) (address-to-string (make-address x)))
                      '(#(127 0 0 1) #(255 255 255 255) #(0 0 0 0 0 0 0 0)
                        #(0 0 0 0 0 0 0 1) #(1 0 0 0 0 0 0 0)))
              '("127.0.0.1" "255.255.255.255" "::" "::1" "1::"))))

(test vector-to-colon-separated.1
  (is (equalp (let ((ip  #(0 0 0 255 255 255 0 0)))
                (values (vector-to-colon-separated ip)
                        (vector-to-colon-separated ip :downcase)
                        (vector-to-colon-separated ip :upcase)))
              (values "::ff:ff:ff:0:" "::ff:ff:ff:0:" "::FF:FF:FF:0:"))))

(test vector-to-colon-separated.2
    (is (string= (vector-to-colon-separated #(1 2 3 4 5 0 6 7))
                 "1:2:3:4:5::6:7")))

(test vector-to-colon-separated.3
  (is (string= (vector-to-colon-separated #(0 2 3 4 5 0 6 7))
               ":2:3:4:5::6:7")))

(test vector-to-colon-separated.4
  (is (string= (vector-to-colon-separated #(1 2 3 4 5 0 6 0))
               "1:2:3:4:5::6:")))

(test colon-separated-to-vector.1
  (is (equalp (mapcar #'colon-separated-to-vector
                      '(":ff::ff:" "::" "::1" "1::" ":2:3:4:5:6:7:8" "1:2:3:4:5:6:7:"
                        ":1::2:" "::127.0.0.1" ":1::127.0.0.1"))
              '(#(0 #xff 0 0 0 0 #xff 0)
                #(0 0 0 0 0 0 0 0)
                #(0 0 0 0 0 0 0 1)
                #(1 0 0 0 0 0 0 0)
                #(0 2 3 4 5 6 7 8)
                #(1 2 3 4 5 6 7 0)
                #(0 1 0 0 0 0 2 0)
                #(0 0 0 0 0 0 #x7f00 1)
                #(0 1 0 0 0 0 #x7f00 1)))))

(test address=.1
  (is-true (address= +ipv4-loopback+ (make-address #(127 0 0 1)))))

(test address=.2
  (is-true (address= +ipv6-loopback+ (ensure-address "::1"))))

(test copy-address.1
  (is-true (loop for designator in (list "127.0.0.1" +max-ipv4-value+ "::" "::1")
              for addr1 = (ensure-address designator)
              for addr2 = (ensure-address designator)
              for addr3 = (copy-address addr1)
              always (and (address= addr1 addr2)
                          (address= addr1 addr3)
                          (address= addr2 addr3)))))

(test make-address.1
  (signals type-error
    (make-address 'not-a-valid-designator)))

(test address.unspecified.1
  (is-true (every #'inet-address-unspecified-p
                  (mapcar #'ensure-address '("0.0.0.0" "::" "0:0:0:0:0:0:0:0")))))

(test address.loopback.1
  (is-true (every #'inet-address-loopback-p
                  (mapcar #'ensure-address '("127.0.0.1" "::1" "0:0::1")))))

(test address.multicast.1
  (is-true (every #'inet-address-multicast-p
                  (mapcar #'ensure-address
                          '("224.0.0.0" "224.1.2.3" "232.0.0.127" "239.255.255.255"
                            "ff02::2" "ff0a::114" "ff05::1:3")))))

(test address.ipv6-ipv4-mapped.1
  (is-true (ipv6-ipv4-mapped-p (ensure-address "::ffff:127.0.0.1"))))

(test address.ipv6.1
  (is (equalp (address-to-vector "::1.2.3.4")
              (values #(0 0 0 0 0 0 #x0102 #x0304) :ipv6))))

;;;; Host Lookup

#-no-internet-available
(test lookup-host.1
  (is (equalp (multiple-value-bind (address addresses truename)
                  (lookup-host "a.root-servers.net" :ipv6 nil)
                (declare (ignore addresses))
                (values (address-equal-p address #(198 41 0 4))
                        truename))
              (values t "a.root-servers.net"))))

#-no-internet-available
(test lookup-host.2
  (is-true (string= (nth-value 2 (lookup-host #(198 41 0 4)))
                    "a.root-servers.net")))

;;; These days lots of people seem to be using DNS servers that don't
;;; report resolving failures for non-existing domains.  This test
;;; will fail there.
(test lookup-host.3
  (signals resolver-no-name-error
    (lookup-host "foo.tninkpad.telent.net.")))

(test lookup-host.4
  (is-true (address-equal-p (lookup-host #(127 0 0 1) :ipv6 nil)
                            #(127 0 0 1))))

(test lookup-host.5
  (signals parse-error
    (lookup-host #(127 0 0))))

(test lookup-host.6
  (signals resolver-no-name-error
    (lookup-host #(127 0 0 1) :ipv6 :ipv6)))

;;;; Service Lookup

(test lookup-service.1
  (is (equalp (lookup-service "ssh")
              (values 22 "ssh" :tcp))))

(test lookup-service.2
  (is (equalp (lookup-service 22 :udp)
              (values 22 "ssh" :udp))))

;;; looks up a reserved service port
(test lookup-service.3
  ;; TODO: check for a more specific error.
  (signals unknown-service
    (lookup-service 1024)))

;;;; Protocol Lookup

(test lookup-protocol.1
  (is (equalp (multiple-value-bind (number name)
                  (lookup-protocol "tcp")
                (values number name))
              (values 6 "tcp"))))

(test lookup-protocol.2
  (is (equalp (multiple-value-bind (number name)
                  (lookup-protocol "udp")
                (values number name))
              (values 17 "udp"))))

(test lookup-protocol.3
  (signals unknown-protocol
    (lookup-protocol "nonexistent-protocol")))

(test lookup-protocol.4
  (is-true (string= (nth-value 1 (lookup-protocol 6))
                    "tcp")))

;;;; Network Interfaces

(test list-network-interfaces.1
  (is-true (<= 1 (length (list-network-interfaces)))))

(test network-interfaces.1
  (is-true
   (flet ((nif-equal (if1 if2)
            (check-type if1 cons)
            (check-type if2 cons)
            (and (string= (interface-name if1) (interface-name if2))
                 (eql (interface-index if1) (interface-index if2)))))
     (loop for nif in (list-network-interfaces)
        always (and (nif-equal nif (lookup-interface (interface-name nif)))
                    (nif-equal nif (lookup-interface (interface-index nif))))))))

;;;; Sockets

;;; RT: don't accept unknown keyword arguments, such as typos.
(test make-socket.1
  (signals error
    (make-socket :this-kw-arg-doesnt-exist t)))

(test make-socket.2
  (is (equalp (with-open-socket (s :address-family :ipv4)
                (values (socket-connected-p s)
                        (socket-open-p s)
                        (> (socket-os-fd s) 1)
                        (socket-address-family s)
                        (socket-protocol s)))
              (values nil t t :ipv4 :default)))) ; why isn't it :TCP?

(test make-socket.3
  (is-true (with-open-socket (s :address-family :ipv4)
             (typep s 'socket))))

;;; Given the functions we've got so far, if you can think of a better
;;; way to make sure the bind succeeded than trying it twice, let me
;;; know. 1974 has no special significance, unless you're the same age
;;; as me.
(test inet.socket-bind.1
  (signals socket-address-in-use-error
    (with-open-socket (s :address-family :ipv4 :connect :passive
                         :local-host #(127 0 0 1) :local-port 1974)
      (with-open-socket (s :address-family :ipv4 :connect :passive
                           :local-host #(127 0 0 1) :local-port 1974)))))

(test sockopt.1
  (is-true (with-open-socket (s :address-family :ipv4)
             (setf (socket-option s :reuse-address) t)
             (socket-option s :reuse-address))))

;;; Like READ-SEQUENCE, but returns early if the full quantity of data
;;; isn't there to be read.  Blocks if no input at all.
(defun read-buf-nonblock (buffer stream)
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

(test simple-tcp-client
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :address-family :ipv4)
     (let ((data (make-string 200)))
       (format s "here is some text")
       (finish-output s)
       (let ((data (subseq data 0 (read-buf-nonblock data s))))
         ;; (format t "~&Got ~S back from TCP echo server~%" data)
         (> (length data) 0))))))

(test sockaddr-return-type
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :address-family :ipv4)
     (and (ipv4-address-p (remote-host s))
          (numberp (remote-port s))))))

;;; We don't support streams with UDP sockets ATM.  But when we do,
;;; let's add a similar test using stream functions.
;;;
;;; FIXME: figure out why this test blocks with the inetd services on
;;; my machines, on both Darwin and Linux/x86-64.  Works with
;;; echo-server.c though --luis
(test simple-udp-client.1
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :type :datagram :address-family :ipv4)
     (send-to s "here is some text")
     (let ((nbytes (nth-value 1 (receive-from s :size 200))))
       (plusp nbytes)))))

(test simple-udp-client.2
  (is-true
   (with-open-socket (s :type :datagram :address-family :ipv4)
     (send-to s "here is some more text"
              :remote-host *echo-address*
              :remote-port *echo-port*)
     (let ((nbytes (nth-value 1 (receive-from s :size 200))))
       (plusp nbytes)))))

(test simple-local-sockets
  (is (string= (let ((file (namestring
                            (make-pathname :name "local-socket" :type nil
                                           :defaults (truename
                                                      (asdf:system-definition-pathname
                                                       (asdf:find-system '#:iolib-tests)))))))
                 ;; (ignore-errors (delete-file file))
                 (with-open-socket (p :address-family :local :connect :passive :local-filename file)
                   (with-open-socket (a :address-family :local :remote-filename file)
                     (format a "local socket test")
                     (finish-output a))
                   (let ((s (accept-connection p)))
                     (prog1 (read-line s)
                       (close s)
                       (delete-file file)))))
               "local socket test")))

(defmacro with-http-stream ((var host port request) &body body)
  `(with-open-socket (,var :address-family :ipv4 :remote-host ,host :remote-port ,port)
     (format ,var ,(concatenate 'string request " HTTP/1.0~%~%"))
     (finish-output ,var)
     ,@body))

#-no-internet-available
(test simple-http-client
  (is-true
   (with-http-stream (s "www.google.com" 80 "HEAD /")
     (let ((data (make-string 200)))
       (setf data (subseq data 0 (read-buf-nonblock data s)))
       ;; (princ data)
       (> (length data) 0)))))

#-no-internet-available
(test sockopt-receive-buffer
  ;; on Linux x86, the receive buffer size appears to be doubled in the
  ;; kernel: we set a size of x and then getsockopt() returns 2x.
  ;; This is why we compare with >= instead of =
  (is-true
   (with-http-stream (s "www.google.com" 80 "HEAD/")
     (setf (socket-option s :receive-buffer) 1975)
     (let ((data (make-string 200)))
       (setf data (subseq data 0 (read-buf-nonblock data s)))
       (and (> (length data) 0)
            (>= (socket-option s :receive-buffer) 1975))))))

(test socket-open-p.1
  (is-true (with-open-socket (s)
             (socket-open-p s))))

(test socket-open-p.2
  (is-true (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                                :address-family :ipv4)
             (socket-open-p s))))

(test socket-open-p.3
  (is-false (with-open-socket (s)
              (close s)
              (socket-open-p s))))

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
  (with-open-socket (s :type :datagram :local-port port)
    (loop
       (multiple-value-bind (buf len address port)
           (receive-from s :size 500)
         (format t "Received ~A bytes from ~A:~A - ~A ~%"
                 len address port (subseq buf 0 (min 10 len)))))))
