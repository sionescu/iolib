;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:common-lisp-user)

(defpackage #:net.sockets
  (:nicknames #:sockets)
  (:use #:common-lisp #:sb-alien)
  (:export
   ;; conditions
   #:possible-bug #:invalid-argument
   #:system-error #:network-error #:resolver-error
   #:resolver-again-error #:resolver-fail-error
   #:resolver-no-name-error #:resolver-no-service-error
   #:unknown-interface #:unknown-protocol
   #:invalid-address
   ; condition accessors
   #:bug-data #:invalid-argument
   #:system-error-code #:system-error-identifier #:resolver-data
   #:interface-name #:interface-index #:protocol-name #:protocol-number
   #:address #:address-type

   ;; low-level address conversion functions
   #:vector-to-ipaddr #:ipaddr-to-vector #:ipaddr-to-dotted
   #:dotted-to-ipaddr #:dotted-to-vector #:vector-to-dotted
   #:colon-separated-to-vector #:vector-to-colon-separated

   ;; addresses
   #:netaddr #:ipv4addr #:ipv6addr #:unixaddr
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
   #:ipv4-address-p #:ipv6-address-p #:unix-address-p
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
))
