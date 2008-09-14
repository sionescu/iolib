;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Base socket classes.
;;;

(in-package :net.sockets)

;;;; Sockets

(defclass socket (dual-channel-single-fd-mixin)
  ((address-family :initarg :address-family :accessor socket-address-family)
   (protocol :initarg :protocol :accessor socket-protocol)
   (bound    :initform nil      :reader   socket-bound-p :type boolean))
  (:documentation "Base class for sockets."))
(unset-method-docstring #'socket-address-family () '(socket))
(set-function-docstring 'socket-address-family "Return the address family of a socket.")
(unset-method-docstring #'socket-protocol () '(socket))
(set-function-docstring 'socket-protocol "Return the protocol of a socket.")

(defgeneric socket-os-fd (socket)
  (:documentation "Returns the OS file descriptor of `SOCKET'."))

(defgeneric socket-type (socket)
  (:documentation "Returns the socket type of `SOCKET' - one of :STREAM or :DATAGRAM."))

(defgeneric socket-open-p (socket)
  (:documentation "Returns a boolean indicating whether or not the file descriptor of `SOCKET' is open."))

(defgeneric local-name (socket)
  (:documentation "For INTERNET sockets, returns two values: the local host and the local port.
For LOCAL sockets, returns the local filename."))

(defgeneric local-host (socket)
  (:documentation "Returns the local host of `SOCKET'.
Works only on INTERNET sockets."))

(defgeneric local-port (socket)
  (:documentation "Returns the local port of `SOCKET' - an (unsigned-byte 16).
Works only on INTERNET sockets."))

(defgeneric local-filename (socket)
  (:documentation "Returns the local filename of `SOCKET'.
Works only on LOCAL sockets."))

(defgeneric remote-name (socket)
  (:documentation "For INTERNET sockets, returns two values: the remote host and the remote port.
For REMOTE sockets, returns the remote filename."))

(defgeneric remote-host (socket)
  (:documentation "Returns the remote host of `SOCKET'.
Works only on INTERNET sockets."))

(defgeneric remote-port (socket)
  (:documentation "Returns the remote port of `SOCKET' - an (unsigned-byte 16).
Works only on INTERNET sockets."))

(defgeneric remote-filename (socket)
  (:documentation "Returns the remote filename of `SOCKET'.
Works only on LOCAL sockets."))

(defgeneric socket-option (socket option-name)
  (:documentation "Returns the value(s) of OS options on `SOCKET'.
For a complete list of supported options see net.sockets/socket-options.lisp ."))

(defclass stream-socket (socket) ()
  (:default-initargs :type :stream)
  (:documentation "Mixin for sockets of type SOCK_STREAM."))

(defclass datagram-socket (socket) ()
  (:default-initargs :type :datagram)
  (:documentation "Mixin for sockets of type SOCK_DGRAM."))

(defgeneric disconnect (socket)
  (:documentation "Disassociates `SOCKET' from any remote address.
Works only on DATAGRAM sockets."))

(define-symbol-macro +default-inet-address-family+
    (if *ipv6* :ipv6 :ipv4))

(defclass internet-socket (socket) ()
  (:default-initargs :address-family +default-inet-address-family+)
  (:documentation "Mixin for sockets of domain AF_INET or AF_INET6."))

(defclass local-socket (socket) ()
  (:default-initargs :address-family :local)
  (:documentation "Mixin for sockets of domain AF_LOCAL."))

(defgeneric send-file-descriptor (socket file-descriptor)
  (:documentation "Send `FILE-DESCRIPTOR' through `SOCKET'.
The receiving process must use RECEIVE-FILE-DESCRIPTOR to receive the
file descriptor in order for it to be valid in the receiving process."))

(defgeneric receive-file-descriptor (socket)
  (:documentation "Receive a file descriptor as ancillary data through `SOCKET'."))

(defun socket-read-fn (fd buffer nbytes)
  (%recvfrom fd buffer nbytes 0 (null-pointer) (null-pointer)))

(defun socket-write-fn (fd buffer nbytes)
  (%sendto fd buffer nbytes 0 (null-pointer) 0))

(defclass active-socket (socket dual-channel-gray-stream) ()
  (:default-initargs :read-fn 'socket-read-fn
                     :write-fn 'socket-write-fn)
  (:documentation "Mixin class for active(client) sockets."))

(defgeneric connect (socket address &key &allow-other-keys)
  (:documentation "Connects `SOCKET' to `ADDRESS'. For INTERNET sockets you can specify
the port to connect to using keyword argument `PORT'. The default value of `PORT' is 0,
which usually means letting the OS choose a random port to connect to.
For `INTERNET' sockets, if `WAIT' is true and a connection cannot be established within
`TIMEOUT' seconds signal `IOMUX:POLL-TIMEOUT', but it works only with non-blocking sockets."))

(defgeneric socket-connected-p (socket)
  (:documentation "Returns a boolean specifying whether or not `SOCKET' is connected."))

(defgeneric shutdown (socket &key read write)
  (:documentation "Shut down all or part of a connection. If `READ' it non-NIL, further receptions are
disallowed; if `WRITE' is non-NIL, further transmissions are disallowed. CLOSE must still be called on
`SOCKET' in order to release OS resources."))

(defgeneric receive-from (socket &rest args &key &allow-other-keys)
  (:documentation "Receives data from `SOCKET'. If `BUFFER' is specified
`START' and `END' are used as bounding index. In that case `BUFFER' must be
an array and its ARRAY-ELEMENT-TYPE be either (UNSIGNED-BYTE 8) or T.
If `BUFFER' is not specified an (UNSIGNED-BYTE 8) buffer of size `SIZE'
will be allocated.

Some flags can also be passed to recvfrom(2):
* `OUT-OF-BAND' for receiving out-of-band data - only for stream sockets
* `PEEK' for keeping the returned data in the kernel buffers
* `WAIT-ALL' for waiting until the entire buffer can be filled
* `DONT-WAIT' for making only the current call non-blocking

The first two values returned are the buffer and the number of elements that have been copied into the buffer.
For INTERNET DATAGRAM sockets, two additional values are returned: the host and port of the remote peer
from which the data was received.
For LOCAL DATAGRAM sockets, one additional values is returned: the filename of the remote peer
from which the data was received."))

(defgeneric send-to (socket buffer &rest args &key &allow-other-keys)
  (:documentation "Send the contents of `BUFFER' to `SOCKET'.
`BUFFER' must be a vector that can be coerced to a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *).
`START' and `END' are used a bounding index on `BUFFER'.
For disconnected datagram sockets, `REMOTE-HOST' and `REMOTE-PORT' or `REMOTE-FILENAME' are used
as destination for the data.

Some flags can also be passed to sendto(2):
* `OUT-OF-BAND' for receiving out-of-band data - only for stream sockets
* `DONT-WAIT' for making only the current call non-blocking
* `DONT-ROUTE' for sending only to hosts on directly connected networks, not using gateways
* `CONFIRM' for signalling progress on the link layer - only available on Linux and only with DATAGRAM sockets
* `MORE' for telling the kernel that there is more data to send - only available on Linux

Returns the number of bytes sent."))

(defclass passive-socket (socket)
  ((listening :initform nil :reader socket-listening-p :type boolean)
   (external-format :initarg :external-format :reader external-format-of)
   (active-class :initarg :active-class :reader active-class
                 :type symbol :allocation :class))
  (:default-initargs :external-format :default)
  (:documentation "Mixin class for passive(server) sockets."))

(defgeneric bind-address (socket address &key &allow-other-keys)
  (:documentation "Sets the local address of `SOCKET' to `ADDRESS'(and `PORT' for INTERNET sockets).
`REUSE-ADDRESS' sets the SO_REUSEADDR socket option on `SOCKET'."))

(defgeneric listen-on (socket &key &allow-other-keys)
  (:documentation "Start allowing incoming connections on `SOCKET'.
`BACKLOG' specifies the maximum length of the queue of pending connections."))

(defgeneric accept-connection (passive-socket &key &allow-other-keys)
  (:documentation "Returns one connection from the queue of pending connections on `SOCKET'.
If `WAIT' is true, waits until a connection is received or `TIMEOUT' expires in which case returns NIL.
If `WAIT' is false and there are no pending connections return NIL.
`EXTERNAL-FORMAT' optionally specifies the external format of the new socket - the default being
that of `SOCKET'. Buffer sizes for the new socket can also be specified using `INPUT-BUFFER-SIZE'
and `OUTPUT-BUFFER-SIZE'."))

(defclass socket-stream-internet-active
    (active-socket stream-socket internet-socket) ()
  (:documentation "Class representing active sockets of type SOCK_STREAM and domain AF_INET or AF_INET6."))

(defclass socket-stream-internet-passive
    (passive-socket stream-socket internet-socket) ()
  (:default-initargs :active-class 'socket-stream-internet-active)
  (:documentation "Class representing passive sockets of type SOCK_STREAM and domain AF_INET or AF_INET6."))

(defclass socket-stream-local-active
    (active-socket stream-socket local-socket) ()
  (:documentation "Class representing active sockets of type SOCK_STREAM and domain AF_LOCAL."))

(defclass socket-stream-local-passive
    (passive-socket stream-socket local-socket) ()
  (:default-initargs :active-class 'socket-stream-local-active)
  (:documentation "Class representing passive sockets of type SOCK_STREAM and domain AF_LOCAL."))

(defclass socket-datagram-internet-active
    (active-socket datagram-socket internet-socket) ()
  (:documentation "Class representing active sockets of type SOCK_DGRAM and domain AF_INET or AF_INET6."))

(defclass socket-datagram-local-active
    (active-socket datagram-socket local-socket) ()
  (:documentation "Class representing active sockets of type SOCK_DGRAM and domain AF_LOCAL."))
