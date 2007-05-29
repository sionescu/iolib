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

(in-package :common-lisp-user)

(defpackage :net.sockets
  (:nicknames #:sockets)
  (:use #:common-lisp #:cffi #:split-sequence
        #:iolib-utils #:io.encodings #:io.streams)
  (:import-from #:iolib-posix
                #:system-error #:unix-error #:message)
  (:export
   ;; conditions
   #:possible-bug
   #:system-error #:unix-error #:resolver-error
   #:unknown-interface #:unknown-protocol
   ; condition accessors
   #:bug-data
   #:error-code #:error-identifier #:error-message
   #:interface-name #:interface-index #:protocol-name #:protocol-number
   #:address #:address-type

   ;; low-level address conversion functions
   #:vector-to-ipaddr #:ipaddr-to-vector #:ipaddr-to-dotted
   #:dotted-to-ipaddr #:dotted-to-vector #:vector-to-dotted
   #:colon-separated-to-vector #:vector-to-colon-separated
   #:string-address-to-vector #:address-to-vector

   ;; addresses
   #:sockaddr #:inetaddr #:ipv4addr #:ipv6addr #:localaddr
   #:make-address #:ensure-address #:convert-or-lookup-inet-address
   #:sockaddr= #:sockaddr->presentation #:copy-sockaddr
   #:name #:abstract-p

   ;; well-known addresses and important values
   #:+max-ipv4-value+
   #:+ipv4-unspecified+ #:+ipv4-loopback+
   #:+ipv6-unspecified+ #:+ipv6-loopback+
   #:+ipv6-interface-local-all-nodes+ #:+ipv6-link-local-all-nodes+
   #:+ipv6-interface-local-all-routers+ #:+ipv6-link-local-all-routers+
   #:+ipv6-site-local-all-routers+

   ;; address predicates
   #:sockaddrp
   #:ipv4-address-p #:ipv6-address-p #:local-address-p
   #:inetaddr-unspecified-p #:inetaddr-loopback-p
   #:inetaddr-multicast-p #:inetaddr-unicast-p
   #:ipv6-ipv4-mapped-p #:ipv6-interface-local-multicast-p
   #:ipv6-link-local-multicast-p #:ipv6-admin-local-multicast-p
   #:ipv6-site-local-multicast-p #:ipv6-organization-local-multicast-p
   #:ipv6-global-multicast-p #:ipv6-reserved-multicast-p
   #:ipv6-unassigned-multicast-p #:ipv6-transient-multicast-p
   #:ipv6-solicited-node-multicast-p
   #:ipv6-link-local-unicast-p #:ipv6-site-local-unicast-p
   #:ipv6-global-unicast-p
   #:ipv6-multicast-type #:inetaddr-type

   ;; hostname, service and protocol lookup
   #:*ipv6*
   #:host #:make-host #:lookup-host #:random-address
   #:host-truename #:host-aliases #:host-addresses
   #:service #:make-service #:lookup-service
   #:service-name #:service-port #:service-protocol
   #:protocol #:make-protocol #:lookup-protocol
   #:protocol-name #:protocol-aliases #:protocol-number

   ;; network interface lookup
   #:interface #:make-interface #:lookup-interface #:get-network-interfaces
   #:interface-name #:interface-index

   ;; socket classes
   #:socket #:stream-socket #:datagram-socket
   #:internet-socket #:local-socket
   #:active-socket #:passive-socket
   #:socket-stream-internet-active
   #:socket-stream-internet-passive
   #:socket-stream-local-active
   #:socket-stream-local-passive
   #:socket-datagram-local-active
   #:socket-datagram-internet-active

   ;; socket methods
   #:socket-fd #:socket-address #:socket-family #:socket-protocol
   #:get-socket-option #:set-socket-option
   #:socket-type #:create-socket #:make-socket #:with-socket
   #:socket-open-p #:local-name #:remote-name
   #:bind-address #:socket-listen #:accept-connection
   #:connect  #:unconnect #:socket-connected-p
   #:shutdown #:socket-send  #:socket-receive
   #:*no-sigpipe* #:*default-backlog-size*))
