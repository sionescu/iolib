;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:common-lisp-user)

(defpackage #:net.sockets
  (:nicknames #:sockets)
  (:use #:common-lisp #:sb-alien)
  (:import-from #:iolib-alien-ng
                #:system-error #:unix-error #:message)
  (:export
   ;; conditions
   #:possible-bug #:invalid-argument
   #:system-error #:unix-error #:resolver-error
   #:unknown-interface #:unknown-protocol
   #:invalid-address
   ; condition accessors
   #:bug-data #:invalid-argument
   #:error-code #:error-identifier #:error-message
   #:interface-name #:interface-index #:protocol-name #:protocol-number
   #:address #:address-type

   ;; low-level address conversion functions
   #:vector-to-ipaddr #:ipaddr-to-vector #:ipaddr-to-dotted
   #:dotted-to-ipaddr #:dotted-to-vector #:vector-to-dotted
   #:colon-separated-to-vector #:vector-to-colon-separated

   ;; addresses
   #:netaddr #:ipv4addr #:ipv6addr #:localaddr
   #:make-address #:netaddr= #:netaddr->presentation #:copy-netaddr
   #:name #:abstract-p

   ;; well-known addresses and important values
   #:+max-ipv4-value+
   #:+ipv4-unspecified+ #:+ipv4-loopback+
   #:+ipv6-unspecified+ #:+ipv6-loopback+
   #:+ipv6-interface-local-all-nodes+ #:+ipv6-link-local-all-nodes+
   #:+ipv6-interface-local-all-routers+ #:+ipv6-link-local-all-routers+
   #:+ipv6-site-local-all-routers+

   ;; address predicates
   #:ipv4-address-p #:ipv6-address-p #:local-address-p
   #:netaddr-unspecified-p #:netaddr-loopback-p
   #:netaddr-multicast-p #:netaddr-unicast-p
   #:ipv6-ipv4-mapped-p #:ipv6-interface-local-multicast-p
   #:ipv6-link-local-multicast-p #:ipv6-admin-local-multicast-p
   #:ipv6-site-local-multicast-p #:ipv6-organization-local-multicast-p
   #:ipv6-global-multicast-p #:ipv6-reserved-multicast-p
   #:ipv6-unassigned-multicast-p #:ipv6-transient-multicast-p
   #:ipv6-solicited-node-multicast-p
   #:ipv6-link-local-unicast-p #:ipv6-site-local-unicast-p
   #:ipv6-global-unicast-p
   #:ipv6-multicast-type #:netaddr-type

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

   ;; classes
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
   #:socket-type #:make-socket #:socket-close #:socket-open-p
   #:socket-non-blocking-mode #:local-name #:remote-name
   #:bind-address #:socket-listen #:accept-connection #:connect
   #:shutdown #:socket-send  #:socket-receive #:unconnect
))
