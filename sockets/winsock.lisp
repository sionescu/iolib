;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; winsock.lisp --- CFFI bindings specific to Winsock.
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

(in-package :net.sockets)

(defctype dword :unsigned-long)
(defctype word :unsigned-short)
(defctype group :unsigned-int)

(osicat-posix::defsyscall "get_osfhandle" :int
  (fd :int))

(osicat-posix::defsyscall "open_osfhandle" :int
  (handle :intptr)
  (flags :int))

(defconstant invalid-socket (1- (expt 2 32)))

(deforeign ("WSASocketA" wsa-socket)
    (errno-wrapper :int :error-predicate (lambda (x) (eql x invalid-socket)))
  (af :int)
  (type :int)
  (protocol :int)
  (protocol-info :pointer)
  (g group)
  (flags dword))

(defun socket (af type proto)
  (open-osfhandle (wsa-socket af type proto (null-pointer) 0 0) 0))

(define-socket-call ("WSAStringToAddressA" wsa-string-to-address) :int
  (address-string :string)
  (address-family :int)
  (protocol-info :pointer)
  (address :pointer)
  (address-length (:pointer :int)))

;;; It's pretty strange that I can't seem to find something equivalent
;;; to strerror().  Thank god for keyboard macros though.
;;; From <http://msdn2.microsoft.com/en-us/library/ms740668.aspx>
(defun get-wsa-error-string (code)
  (case code
    (6 "Specified event object handle is invalid.") ; WSA_INVALID_HANDLE
    (8 "Insufficient memory available.") ; WSA_NOT_ENOUGH_MEMORY
    (87 "One or more parameters are invalid.") ; WSA_INVALID_PARAMETER
    (995 "Overlapped operation aborted.") ; WSA_OPERATION_ABORTED
    (996 "Overlapped I/O event object not in signaled state.")
                                        ; WSA_IO_INCOMPLETE
    (997 "Overlapped operations will complete later.") ; WSA_IO_PENDING
    (10004 "Interrupted function call.") ; WSAEINTR
    (10009 "File handle is not valid.") ; WSAEBADF
    (10013 "Permission denied.")        ; WSAEACCES
    (10014 "Bad address.")              ; WSAEFAULT
    (10022 "Invalid argument.")         ; WSAEINVAL
    (10024 "Too many open sockets.")    ; WSAEMFILE
    (10035 "Resource temporarily unavailable.") ; WSAEWOULDBLOCK
    (10036 "Operation now in progress.") ; WSAEINPROGRESS
    (10037 "Operation already in progress.") ; WSAEALREADY
    (10038 "Socket operation on nonsocket.") ; WSAENOTSOCK
    (10039 "Destination address required.") ; WSAEDESTADDRREQ
    (10040 "Message too long.")         ; WSAEMSGSIZE
    (10041 "Protocol wrong type for socket.") ; WSAEPROTOTYPE
    (10042 "Bad protocol option.")      ; WSAENOPROTOOPT
    (10043 "Protocol not supported.")   ; WSAEPROTONOSUPPORT
    (10044 "Socket type not supported.") ; WSAESOCKTNOSUPPORT
    (10045 "Operation not supported.")  ; WSAEOPNOTSUPP
    (10046 "Protocol family not supported.") ; WSAEPFNOSUPPORT
    (10047 "Address family not supported by protocol family.") ; WSAEAFNOSUPPORT
    (10048 "Address already in use.")   ; WSAEADDRINUSE
    (10049 "Cannot assign requested address.") ; WSAEADDRNOTAVAIL
    (10050 "Network is down.")          ; WSAENETDOWN
    (10051 "Network is unreachable.")   ; WSAENETUNREACH
    (10052 "Network dropped connection on reset.") ; WSAENETRESET
    (10053 "Software caused connection abort.") ; WSAECONNABORTED
    (10054 "Connection reset by peer.") ; WSAECONNRESET
    (10055 "No buffer space available.") ; WSAENOBUFS
    (10056 "Socket is already connected.") ; WSAEISCONN
    (10057 "Socket is not connected.")  ; WSAENOTCONN
    (10058 "Cannot send after socket shutdown.") ; WSAESHUTDOWN
    (10059 "Too many references.")      ; WSAETOOMANYREFS
    (10060 "Connection timed out.")     ; WSAETIMEDOUT
    (10061 "Connection refused.")       ; WSAECONNREFUSED
    (10062 "Cannot translate name.")    ; WSAELOOP
    (10063 "Name too long.")            ; WSAENAMETOOLONG
    (10064 "Host is down.")             ; WSAEHOSTDOWN
    (10065 "No route to host.")         ; WSAEHOSTUNREACH
    (10066 "Directory not empty.")      ; WSAENOTEMPTY
    (10067 "Too many processes.")       ; WSAEPROCLIM
    (10068 "User quota exceeded.")      ; WSAEUSERS
    (10069 "Disk quota exceeded.")      ; WSAEDQUOT
    (10070 "Stale file handle reference.") ; WSAESTALE
    (10071 "Item is remote.")           ; WSAEREMOTE
    (10091 "Network subsystem is unavailable.") ; WSASYSNOTREADY
    (10092 "Winsock.dll version out of range.") ; WSAVERNOTSUPPORTED
    (10093 "Successful WSAStartup not yet performed.") ; WSANOTINITIALISED
    (10101 "Graceful shutdown in progress.") ; WSAEDISCON
    (10102 "No more results.")          ; WSAENOMORE
    (10103 "Call has been canceled.")   ; WSAECANCELLED
    (10104 "Procedure call table is invalid.") ; WSAEINVALIDPROCTABLE
    (10105 "Service provider is invalid.") ; WSAEINVALIDPROVIDER
    (10106 "Service provider failed to initialize.") ; WSAEPROVIDERFAILEDINIT
    (10107 "System call failure.")      ; WSASYSCALLFAILURE
    (10108 "Service not found.")        ; WSASERVICE_NOT_FOUND
    (10109 "Class type not found.")     ; WSATYPE_NOT_FOUND
    (10110 "No more results.")          ; WSA_E_NO_MORE
    (10111 "Call was canceled.")        ; WSA_E_CANCELLED
    (10112 "Database query was refused.") ; WSAEREFUSED
    (11001 "Host not found.")           ; WSAHOST_NOT_FOUND
    (11002 "Nonauthoritative host not found.") ; WSATRY_AGAIN
    (11003 "This is a nonrecoverable error.") ; WSANO_RECOVERY
    (11004 "Valid name, no data record of requested type.") ; WSANO_DATA
    (11005 "QOS receivers.")            ; WSA_QOS_RECEIVERS
    (11006 "QOS senders.")              ; WSA_QOS_SENDERS
    (11007 "No QOS senders.")           ; WSA_QOS_NO_SENDERS
    (11008 "QOS no receivers.")         ; WSA_QOS_NO_RECEIVERS
    (11009 "QOS request confirmed.")    ; WSA_QOS_REQUEST_CONFIRMED
    (11010 "QOS admission error.")      ; WSA_QOS_ADMISSION_FAILURE
    (11011 "QOS policy failure.")       ; WSA_QOS_POLICY_FAILURE
    (11012 "QOS bad style.")            ; WSA_QOS_BAD_STYLE
    (11013 "QOS bad object.")           ; WSA_QOS_BAD_OBJECT
    (11014 "QOS traffic control error.") ; WSA_QOS_TRAFFIC_CTRL_ERROR
    (11015 "QOS generic error.")        ; WSA_QOS_GENERIC_ERROR
    (11016 "QOS service type error.")   ; WSA_QOS_ESERVICETYPE
    (11017 "QOS flowspec error.")       ; WSA_QOS_EFLOWSPEC
    (11018 "Invalid QOS provider buffer.") ; WSA_QOS_EPROVSPECBUF
    (11019 "Invalid QOS filter style.") ; WSA_QOS_EFILTERSTYLE
    (11020 "Invalid QOS filter type.")  ; WSA_QOS_EFILTERTYPE
    (11021 "Incorrect QOS filter count.") ; WSA_QOS_EFILTERCOUNT
    (11022 "Invalid QOS object length.") ; WSA_QOS_EOBJLENGTH
    (11023 "Incorrect QOS flow count.") ; WSA_QOS_EFLOWCOUNT
    (11024 "Unrecognized QOS object.")  ; WSA_QOS_EUNKOWNPSOBJ
    (11025 "Invalid QOS policy object.") ; WSA_QOS_EPOLICYOBJ
    (11026 "Invalid QOS flow descriptor.") ; WSA_QOS_EFLOWDESC
    (11027 "Invalid QOS provider-specific flowspec.") ; WSA_QOS_EPSFLOWSPEC
    (11028 "Invalid QOS provider-specific filterspec.") ; WSA_QOS_EPSFILTERSPEC
    (11029 "Invalid QOS shape discard mode object.") ; WSA_QOS_ESDMODEOBJ
    (11030 "Invalid QOS shaping rate object.") ; WSA_QOS_ESHAPERATEOBJ
    (11031 "Reserved policy QOS element type.") ; WSA_QOS_RESERVED_PETYPE
    (t "Unknown Winsock error.")))

;;; not actually used (yet?)
(defcstruct wsa-data
  (version word)
  (high-version word)
  (description :char :count 257)
  (system-status :char :count 129)
  (max-sockets :unsigned-short)
  (max-udp-dg :unsigned-short)
  (vendor-info :string))

(define-socket-call ("WSAStartup" %wsa-startup) :int
  (version-requested word)
  (data wsa-data))

(defun wsa-startup (version-requested)
  (with-foreign-object (data 'wsa-data)
    (%wsa-startup version-requested data)))

(defun make-wsa-version (major minor)
  (dpb minor (byte 8 8) major))

(defvar *wsa-startup-call* (wsa-startup (make-wsa-version 2 2)))

;;;; Network Info

(defconstant max-hostname-len 128)
(defconstant max-domain-name-len 128)
(defconstant max-scope-id-len 256)

(defcstruct ip-address-string
  (string :char :count #.(* 4 4)))

(defctype ip-mask-string ip-address-string)

(defcstruct ip-addr-string
  (next :pointer)
  (ip-address ip-address-string)
  (ip-mask ip-mask-string)
  (context dword))

(defcstruct fixed-info
  (host-name :char :count #.(+ max-hostname-len 4))
  (domain-name :char :count #.(+ max-domain-name-len 4))
  (current-dns-server (:pointer ip-addr-string))
  (dns-server-list ip-addr-string)
  (node-type :uint)
  (scope-id :char :count #.(+ max-scope-id-len 4))
  (enable-routing :uint)
  (enable-proxy :uint)
  (enable-dns :uint))

(load-foreign-library "Iphlpapi.dll")

(deforeign ("GetNetworkParams" %get-network-params) dword
  (fixed-info fixed-info)
  (out-buf-len (:pointer :ulong)))

(defconstant error-success 0)
(defconstant error-buffer-overflow 111)

;;; just getting the DNS servers for now.
(defun get-first-dns-server ()
  (with-foreign-object (len :ulong)
    (assert (eql error-buffer-overflow
                 (%get-network-params (null-pointer) len)))
    (with-foreign-pointer (ptr (mem-ref len :ulong))
      (assert (eql error-success (%get-network-params ptr len)))
      (values
       (foreign-string-to-lisp
        (foreign-slot-pointer (foreign-slot-value
                               ptr 'fixed-info 'dns-server-list)
                              'ip-addr-string 'ip-address)
        :encoding :ascii)))))
