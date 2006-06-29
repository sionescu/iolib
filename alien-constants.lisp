;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;; adapted from constants.lisp in the sb-bsd-sockets contrib module
;; many docstrings here are Copyright (C) 2001-2003 by
;; the Institute of Electrical and Electronics Engineers, Inc and The Open Group


("sys/types.h" "sys/socket.h" "unistd.h" "sys/un.h"
 "netinet/in.h" "netinet/in_systm.h" "netinet/ip.h" "sys/ioctl.h"
 "net/if.h" "netdb.h" "errno.h" "netinet/tcp.h" "fcntl.h" "limits.h"
 "sys/uio.h" "arpa/inet.h" "sys/select.h" "time.h"
 #+linux "netpacket/packet.h" #+linux "net/ethernet.h"
 #+linux "asm/types.h" #+linux "linux/netlink.h" #+linux "linux/rtnetlink.h")

(;;
 ;; from sys/types.h
 ;;
 (:type size-t "size_t")
 (:type ssize-t "ssize_t")
 (:type off-t "off_t")
 (:type pid-t "pid_t")
 (:type uid-t "uid_t")
 (:type gid-t "gid_t")

 ;;
 ;; from inttypes.h
 ;;
 (:type uint8-t "uint8_t")
 (:type uint16-t "uint16_t")
 (:type uint32-t "uint32_t") 

 ;;
 ;; from sys/uio.h
 ;;
 (:structure iovec ("struct iovec"
                    ((* t) base "void *" "iov_base")
                    (size-t len "size_t" "iov_len")))
#+freebsd (:integer iov-max "IOV_MAX")
#+linux (:integer uio-maxiov "UIO_MAXIOV")

 ;;
 ;; from sys/socket.h
 ;;
 (:type socklen-t "socklen_t")
 (:type sa-family-t "sa_family_t")
 (:structure sockaddr ("struct sockaddr"
                       #+freebsd (uint8-t len "unsigned char" "sa_len")
                       (sa-family-t family "sa_family_t" "sa_family")
                       ((array (unsigned 8)) data "char" "sa_data")))
 (:structure sockaddr-storage ("struct sockaddr_storage"
                               #+freebsd (uint8-t len "unsigned char" "ss_len")
                               (sa-family-t family "sa_family_t" "ss_family")))

 (:integer msg-ctrunc "MSG_CTRUNC")              ; recvmsg
 (:integer msg-dontroute "MSG_DONTROUTE")        ;         sendmsg
 (:integer msg-eor "MSG_EOR")                    ; recvmsg sendmsg
 (:integer msg-oob "MSG_OOB")                    ; recvmsg sendmsg
 (:integer msg-peek "MSG_PEEK")                  ; recvmsg
 (:integer msg-trunc "MSG_TRUNC")                ; recvmsg
 (:integer msg-waitall "MSG_WAITALL")            ; recvmsg
 (:integer msg-dontwait "MSG_DONTWAIT")          ; recvmsg sendmsg
 (:integer msg-nosignal "MSG_NOSIGNAL")          ;         sendmsg
#+linux (:integer msg-errqueue "MSG_ERRQUEUE")   ; recvmsg
#+linux (:integer msg-more "MSG_MORE")           ;         sendmsg
#+linux (:integer msg-confirm "MSG_CONFIRM")     ; sendmsg sendmsg
#+linux (:integer msg-proxy "MSG_PROXY")         ; 
#+linux (:integer msg-fin "MSG_FIN")             ; 
#+linux (:integer msg-syn "MSG_SYN")             ; 
#+freebsd (:integer msg-eof "MSG_EOF")           ; 
#+freebsd (:integer msg-nbio "MSG_NBIO")         ; 
#+freebsd (:integer msg-compat "MSG_COMPAT")     ; 
 (:structure msghdr ("struct msghdr"
                     (c-string-pointer name "void *" "msg_name")
                     (socklen-t namelen "socklen_t" "msg_namelen")
                     ((* (struct iovec)) iov "struct iovec" "msg_iov")
                     (size-t iovlen "size_t" "msg_iovlen")
                     ((* t) control "void *" "msg_control")
                     (socklen-t controllen "socklen_t" "msg_controllen")
                     (int flags "int" "msg_flags")))

 (:integer sol-socket "SOL_SOCKET")
#+linux (:integer sol-tcp "SOL_TCP")
#+linux (:integer sol-ip "SOL_IP")
#+linux (:integer sol-raw "SOL_RAW")

 (:structure cmsghdr ("struct cmsghdr"
                      (socklen-t len "socklen_t" "cmsg_len")
                      (int level "int" "cmsg_level")
                      (int type "int" "cmsg_type")))
#+freebsd
 (:integer cmgroup-max "CMGROUP_MAX")
#+freebsd
 (:structure cmsgcred ("struct cmsgcred"
                       (pid-t pid "pid_t" "cmcred_pid")
                       (uid-t uid "uid_t" "cmcred_uid")
                       (uid-t euid "uid_t" "cmcred_euid")
                       (gid-t gid "gid_t" "cmcred_gid")
                       (short ngroups "short" "cmcred_ngroups")
                       ((array gid-t) groups "gid_t" "cmcred_groups")))
 

 (:integer scm-rights "SCM_RIGHTS")
#+linux (:integer scm-credentials "SCM_CREDENTIALS")
#+linux (:structure ucred ("struct ucred"
                           (pid-t pid "pid_t" "pid")
                           (uid-t uid "uid_t" "uid")
                           (gid-t gid "gid_t" "gid")))
#+freebsd
 (:structure sockcred ("struct sockcred"
                       (uid-t uid "uid_t" "sc_uid")
                       (uid-t euid "uid_t" "sc_euid")
                       (gid-t gid "gid_t" "sc_gid")
                       (gid-t egid "gid_t" "sc_egid")
                       (int ngroups "int" "sc_ngroups")
                       ((array gid-t) groups "gid_t" "sc_groups")))

 (:structure linger ("struct linger"
                     (int onoff "int" "l_onoff")
                     (int linger "int" "l_linger")))
#+freebsd
 (:structure accept-filter-arg ("struct accept_filter_arg"
                                ((array char 16) name "char" "af_name")
                                ((array char 240) arg "char" "af_arg")))
 

 ;; socket families
 (:integer af-unspec "AF_UNSPEC")
 (:integer af-local "AF_UNIX")
 (:integer af-unix "AF_UNIX")
 (:integer af-inet "AF_INET")
 (:integer af-inet6 "AF_INET6")
 (:integer af-route "AF_ROUTE")
 (:integer af-key #+linux "AF_KEY"
                  #+freebsd "PF_KEY")
#+linux (:integer af-netlink "AF_NETLINK")
#+linux (:integer af-packet "AF_PACKET")

 ;; socket types
 (:integer sock-stream "SOCK_STREAM")
 (:integer sock-dgram "SOCK_DGRAM")
 (:integer sock-seqpacket "SOCK_SEQPACKET")
 (:integer sock-raw "SOCK_RAW")
#+linux (:integer sock-rdm "SOCK_RDM")

 (:integer somaxconn "SOMAXCONN")

 (:integer so-acceptconn "SO_ACCEPTCONN")            ; get
#+linux (:integer so-bsdcompat "SO_BSDCOMPAT")       ;     set
#+linux (:integer so-bindtodevice "SO_BINDTODEVICE") ;     set
 (:integer so-broadcast "SO_BROADCAST")              ; get set
 (:integer so-debug "SO_DEBUG")                      ; get set
 (:integer so-dontroute "SO_DONTROUTE")              ; get set
 (:integer so-error "SO_ERROR")                      ; get
 (:integer so-keepalive "SO_KEEPALIVE")              ; get set
 (:integer so-linger "SO_LINGER")                    ; get set
 (:integer so-oobinline "SO_OOBINLINE")              ; get set
#+linux (:integer so-passcred "SO_PASSCRED")         ; get set
#+linux (:integer so-peercred "SO_PEERCRED")         ; get
#+linux (:integer so-priority "SO_PRIORITY")         ; get set
 (:integer so-rcvbuf "SO_RCVBUF")                    ; get set
#+linux (:integer so-rcvbufforce "SO_RCVBUFFORCE")   ; get set
 (:integer so-rcvlowat "SO_RCVLOWAT")                ; get set
 (:integer so-rcvtimeo "SO_RCVTIMEO")                ; get set
 (:integer so-reuseaddr "SO_REUSEADDR")              ; get set
 (:integer so-sndbuf "SO_SNDBUF")                    ; get set
#+linux (:integer so-sndbufforce "SO_SNDBUFFORCE")   ; get set
 (:integer so-sndlowat "SO_SNDLOWAT")                ; get set
 (:integer so-sndtimeo "SO_SNDTIMEO")                ; get set
 (:integer so-timestamp "SO_TIMESTAMP")              ; get set
 (:integer so-type "SO_TYPE")                        ; get
#+freebsd (:integer so-useloopback "SO_USELOOPBACK")
#+freebsd (:integer so-reuseport "SO_REUSEPORT")
#+freebsd (:integer so-nosigpipe "SO_NOSIGPIPE")
#+freebsd (:integer so-acceptfilter "SO_ACCEPTFILTER")
#+freebsd (:integer so-bintime "SO_BINTIME")
#+freebsd (:integer so-label "SO_LABEL")
#+freebsd (:integer so-peerlabel "SO_PEERLABEL")
#+freebsd (:integer so-listenqlimit "SO_LISTENQLIMIT")
#+freebsd (:integer so-listenqlen "SO_LISTENQLEN")
#+freebsd (:integer so-listenincqlen "SO_LISTENINCQLEN")

 (:integer shut-rd "SHUT_RD")
 (:integer shut-wr "SHUT_WR")
 (:integer shut-rdwr "SHUT_RDWR")

 ;;
 ;; from sys/un.h
 ;;
 (:structure sockaddr-un ("struct sockaddr_un"
                          #+freebsd (uint8-t len "unsigned char" "sun_len")
                          (sa-family-t family "sa_family_t" "sun_family")
                          ((array (unsigned 8)) path "char" "sun_path")))

#+freebsd (:integer local-peercred "LOCAL_PEERCRED")
#+freebsd (:integer local-creds "LOCAL_CREDS")
#+freebsd (:integer local-connwait "LOCAL_CONNWAIT")

 ;;
 ;; from netinet/in.h
 ;;
 (:type in-port-t "in_port_t")
 (:type in-addr-t "in_addr_t")

 (:structure sockaddr-in ("struct sockaddr_in"
                          #+freebsd (uint8-t len "uint8_t" "sin_len")
                          (sa-family-t family "sa_family_t" "sin_family")
                          (in-port-t port "in_port_t" "sin_port")
                          (in-addr-t addr "struct in_addr" "sin_addr")))

 (:structure in6-addr ("struct in6_addr"
                       ((union nil (addr8 (array uint8-t  16))
                                   (addr16 (array uint16-t 8))
                                   (addr32 (array uint32-t 4)))
                        in6-u "union" #+linux "in6_u"
                                      #+freebsd "__u6_addr")))

 (:structure sockaddr-in6 ("struct sockaddr_in6"
                           #+freebsd (uint8-t len "uint8_t" "sin6_len")
                           (sa-family-t family "sa_family_t" "sin6_family")
                           (in-port-t port "in_port_t" "sin6_port")
                           (uint32-t flowinfo "uint32_t" "sin6_flowinfo")
                           ((struct in6-addr) addr "struct in6_addr" "sin6_addr")
                           (uint32-t scope-id "uint32_t" "sin6_scope_id")))

;;  (:structure ip-mreq ("struct ip_mreq"
;;                       (in-addr-t imr-multiaddr "struct in_addr" "imr_multiaddr")
;;                       (in-addr-t imr-interface "struct in_addr" "imr_interface")))
;;  (:structure ip-mreq-source ("struct ip_mreq_source"
;;                              (in-addr-t imr-multiaddr "struct in_addr" "imr_multiaddr")
;;                              (in-addr-t imr-interface "struct in_addr" "imr_interface")
;;                              (in-addr-t imr-sourceaddr "struct in_addr" "imr_sourceaddr")))
;;  (:structure ipv6-mreq ("struct ipv6_mreq"
;;                         ((array (unsigned 8)) ipv6mr-multiaddr "struct in6_addr" "ipv6mr_multiaddr")
;;                         (unsigned ipv6mr-interface "unsigned" "ipv6mr_interface")))
 
 (:integer inaddr-any "INADDR_ANY")
 (:integer inaddr-broadcast "INADDR_BROADCAST")
 (:integer inaddr-none "INADDR_NONE")
 (:integer in-loopbacknet "IN_LOOPBACKNET")
 (:integer inaddr-loopback "INADDR_LOOPBACK")
 (:integer inaddr-unspec-group "INADDR_UNSPEC_GROUP")
 (:integer inaddr-allhosts-group "INADDR_ALLHOSTS_GROUP")
 (:integer inaddr-allrtrs-group "INADDR_ALLRTRS_GROUP")
 (:integer inaddr-max-local-group "INADDR_MAX_LOCAL_GROUP")

 (:integer ipproto-ip "IPPROTO_IP")
 (:integer ipproto-ipv6 "IPPROTO_IPV6")
 (:integer ipproto-icmp "IPPROTO_ICMP")
 (:integer ipproto-icmpv6 "IPPROTO_ICMPV6")
 (:integer ipproto-raw "IPPROTO_RAW")
 (:integer ipproto-tcp "IPPROTO_TCP")
 (:integer ipproto-udp "IPPROTO_UDP")
 (:integer ipproto-sctp "IPPROTO_SCTP")

 (:integer inet-addrstrlen "INET_ADDRSTRLEN")
 (:integer inet6-addrstrlen "INET6_ADDRSTRLEN")

 (:integer ipv6-join-group "IPV6_JOIN_GROUP")
 (:integer ipv6-leave-group "IPV6_LEAVE_GROUP")
 (:integer ipv6-multicast-hops "IPV6_MULTICAST_HOPS")
 (:integer ipv6-multicast-if "IPV6_MULTICAST_IF")
 (:integer ipv6-multicast-loop "IPV6_MULTICAST_LOOP")
 (:integer ipv6-unicast-hops "IPV6_UNICAST_HOPS")
 (:integer ipv6-v6only "IPV6_V6ONLY")

 ;;
 ;; from netinet/tcp.h
 ;;
 (:integer tcp-nodelay "TCP_NODELAY")
 (:integer tcp-maxseg "TCP_MAXSEG")
#+linux (:integer tcp-cork "TCP_CORK")
#+linux (:integer tcp-keepidle "TCP_KEEPIDLE")
#+linux (:integer tcp-keepintvl "TCP_KEEPINTVL")
#+linux (:integer tcp-keepcnt "TCP_KEEPCNT")
#+linux (:integer tcp-syncnt "TCP_SYNCNT")
#+linux (:integer tcp-linger2 "TCP_LINGER2")
#+linux (:integer tcp-defer-accept "TCP_DEFER_ACCEPT")
#+linux (:integer tcp-window-clamp "TCP_WINDOW_CLAMP")
 (:integer tcp-info "TCP_INFO")
#+linux (:integer tcp-quickack "TCP_QUICKACK")

#+linux
 (:enum connstates ((tcp-established "TCP_ESTABLISHED")
                    (tcp-syn-sent "TCP_SYN_SENT")
                    (tcp-syn-recv "TCP_SYN_RECV")
                    (tcp-fin-wait1 "TCP_FIN_WAIT1")
                    (tcp-fin-wait2 "TCP_FIN_WAIT2")
                    (tcp-time-wait "TCP_TIME_WAIT")
                    (tcp-close "TCP_CLOSE")
                    (tcp-close-wait "TCP_CLOSE_WAIT")
                    (tcp-last-ack "TCP_LAST_ACK")
                    (tcp-listen "TCP_LISTEN")
                    (tcp-closing "TCP_CLOSING")))


 ;;
 ;; from netdb.h
 ;;

 (:integer ipport-reserved "IPPORT_RESERVED")

 (:structure addrinfo ("struct addrinfo"
                       (int flags "int" "ai_flags")
                       (int family "int" "ai_family")
                       (int socktype "int" "ai_socktype")
                       (int protocol "int" "ai_protocol")
                       (socklen-t addrlen "socklen_t" "ai_addrlen")
                       ((* (struct sockaddr)) addr "struct sockaddr *" "ai_addr")
                       (c-string-pointer canonname "char *" "ai_canonname")
                       ((* (struct addrinfo)) next "struct addrinfo *" "ai_next")))

 ;; addrinfo flags
 (:integer ai-passive "AI_PASSIVE")
 (:integer ai-canonname "AI_CANONNAME")
 (:integer ai-numerichost "AI_NUMERICHOST")
 (:integer ai-numericserv "AI_NUMERICSERV")
 (:integer ai-v4mapped "AI_V4MAPPED")
 (:integer ai-all "AI_ALL")
 (:integer ai-addrconfig "AI_ADDRCONFIG")
#+linux (:integer ai-idn "AI_IDN")
#+linux (:integer ai-canonidn "AI_CANONIDN")
#+linux (:integer ai-idn_allow_unassigned "AI_IDN_ALLOW_UNASSIGNED")
#+linux (:integer ai-idn_use_std3_ascii_rules "AI_IDN_USE_STD3_ASCII_RULES")

 (:integer ni-maxhost "NI_MAXHOST")
 (:integer ni-maxserv "NI_MAXSERV")

 ;; nameinfo flags
 (:integer ni-nofqdn "NI_NOFQDN")
 (:integer ni-numerichost "NI_NUMERICHOST")
 (:integer ni-namereqd "NI_NAMEREQD")
 (:integer ni-numericserv "NI_NUMERICSERV")
 (:integer ni-numericscope "NI_NUMERICSCOPE")
 (:integer ni-dgram "NI_DGRAM")
#+linux (:integer ni-canonidn "NI_CANONIDN")
#+linux (:integer ni-idn_allow_unassigned "NI_IDN_ALLOW_UNASSIGNED")
#+linux (:integer ni-idn_use_std3_ascii_rules "NI_IDN_USE_STD3_ASCII_RULES")

 ;; error codes
 (:integer netdb-success "NETDB_SUCCESS")
 (:integer netdb-internal "NETDB_INTERNAL")
#+linux (:integer eai-addrfamily "EAI_ADDRFAMILY" "The specified network host does not have any network addresses in the requested address family.")
 (:integer eai-again "EAI_AGAIN" "The name could not be resolved at this time. Future attempts may succeed.")
 (:integer eai-badflags "EAI_BADFLAGS" "The flags had an invalid value.")
 (:integer eai-fail "EAI_FAIL" "A non-recoverable error occurred.")
 (:integer eai-family "EAI_FAMILY" "The address family was not recognized or the address length was invalid for the specified family.")
 (:integer eai-memory "EAI_MEMORY" "There was a memory allocation failure.")
 (:integer eai-noname "EAI_NONAME" "The name does not resolve for the supplied parameters. NI-NAMEREQD is set and the host's name cannot be located, or both nodename and servname were null.")
 (:integer eai-service "EAI_SERVICE" "The service passed was not recognized for the specified socket type.")
 (:integer eai-socktype "EAI_SOCKTYPE" "The intended socket type was not recognized.")
 (:integer eai-system "EAI_SYSTEM" "A system error occurred. The error code can be found in errno")
#+linux (:integer eai-overflow "EAI_OVERFLOW" "An argument buffer overflowed.")
#+freebsd (:integer eai-badhints "EAI_BADHINTS")
#+freebsd (:integer eai-protocol "EAI_PROTOCOL")
#+freebsd (:integer eai-max "EAI_MAX")
#+linux (:integer eai-inprogress "EAI_INPROGRESS" "Processing request in progress.")
#+linux (:integer eai-canceled "EAI_CANCELED" "Request canceled.")
#+linux (:integer eai-notcanceled "EAI_NOTCANCELED" "Request not canceled.")
#+linux (:integer eai-alldone "EAI_ALLDONE" "All requests done.")
#+linux (:integer eai-intr "EAI_INTR" "Interrupted by a signal.")
#+linux (:integer eai-idn_encode "EAI_IDN_ENCODE" "IDN encoding failed.")

 (:structure protoent ("struct protoent"
                       (c-string-pointer name "char *" "p_name")
                       ((* c-string) aliases "char **" "p_aliases")
                       (int proto "int" "p_proto")))

 ;;
 ;; from net/if.h
 ;;
 (:structure if-nameindex ("struct if_nameindex"
                           (unsigned index "unsigned" "if_index")
                           (c-string-pointer name "char *" "if_name")))
 (:integer ifnamesize "IF_NAMESIZE")
 (:integer ifnamsiz "IFNAMSIZ")


 ;;
 ;; from sys/select.h
 ;;
 (:type suseconds-t "suseconds_t")

 ;;
 ;; from time.h
 ;;
 (:type time-t "time_t")
 (:structure timeval ("struct timeval"
                      (time-t tv-sec "time_t" "tv_sec")
                      (suseconds-t tv-usec "suseconds_t" "tv_usec")))
 ;; THE END
 )
