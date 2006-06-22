 (:integer host-not-found "HOST_NOT_FOUND")
 (:integer no-data "NO_DATA")
 (:integer no-address "NO_ADDRESS")
 (:integer no-recovery "NO_RECOVERY")
 (:integer try-again "TRY_AGAIN")

 (:structure hostent ("struct hostent"
                      (c-string-pointer name "char *" "h_name")
                      ((* c-string) aliases "char **" "h_aliases")
                      (int type "int" "h_addrtype")
                      (int length "int" "h_length")
                      ((* (* (unsigned 8))) addresses "char **" "h_addr_list")))
 (:structure servent ("struct servent"
                       (c-string-pointer name "char *" "s_name")
                       ((* (* (unsigned 8))) aliases "char **" "s_aliases")
                       (int port "int" "s_port")
                       (c-string-pointer proto "char *" "s_proto")))
 (:structure netent ("struct netent"
                     (c-string-pointer name "char *" "n_name")
                     ((* c-string) aliases "char **" "n_aliases")
                     (int type "int" "n_addrtype")
                     (uint32-t net "int" "n_net")))

;; Host lookup
(define-alien-routine "endhostent" void)

(define-alien-routine "gethostbyaddr" (* (struct hostent))
  (addr (* t))
  (len socklen-t)
  (type int))

(define-alien-routine "gethostbyname" (* (struct hostent))
  (name c-string))

(define-alien-routine "gethostent" (* (struct hostent)))

(define-alien-routine "sethostent" void
  (stayopen int))

;; Service lookup
(define-alien-routine "endservent" void)

(define-alien-routine "getservbyport" (* (struct servent))
  (port int)
  (proto c-string))

(define-alien-routine "getservbyname" (* (struct servent))
  (name c-string)
  (proto c-string))

(define-alien-routine "getservent" (* (struct servent)))

(define-alien-routine "setservent" void
  (stayopen int))

;; Net lookup
(define-alien-routine "endnetent" void)

(define-alien-routine "getnetbyaddr" (* (struct netent))
  (net uint32-t)
  (type int))

(define-alien-routine "getnetbyname" (* (struct netent))
  (name c-string))

(define-alien-routine "getnetent" (* (struct netent)))

(define-alien-routine "setnetent" void
  (stayopen int))
