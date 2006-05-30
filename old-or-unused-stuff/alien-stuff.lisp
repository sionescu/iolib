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

(define-alien-routine "endhostent" void)

(define-alien-routine "endservent" void)

(define-alien-routine "gethostbyaddr" (* (struct hostent))
  (addr (* t))
  (len socklen-t)
  (type int))

(define-alien-routine "gethostbyname" (* (struct hostent))
  (name c-string))

(define-alien-routine "gethostent" (* (struct hostent)))

(define-alien-routine "getservbyname" (* (struct servent))
  (name c-string)
  (proto c-string))

(define-alien-routine "getservbyport" (* (struct servent))
  (port int)
  (proto c-string))

(define-alien-routine "getservent" (* (struct servent)))

(define-alien-routine "sethostent" void
  (stayopen int))

(define-alien-routine "setservent" void
  (stayopen int))
