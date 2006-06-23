/*
 * Copyright (c) 2005,2006 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2005 Fraser Research Inc. <djs@fraserresearch.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>

CAMLprim value
ounix_set_tcp_nodelay (value csock, value o)
{
    CAMLparam2 (csock,o);
    int s = Int_val(csock);
    int opt = (Bool_val(o)) ? 1 : 0;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (void *)&opt, sizeof(opt));
    CAMLreturn(Val_unit);
}

value
ounix_set_ip_multicast_ttl (value socket, value ttl)
{
    CAMLparam2(socket, ttl);
    CAMLlocal1(result);

    int fd = Int_val(socket);
    unsigned char t = Int_val(ttl);
    int r = setsockopt(fd, IPPROTO_IP, IP_MULTICAST_TTL, &t, sizeof(t));

    result = Val_int(r);    
    CAMLreturn(result);
}

value
ounix_set_ip_multicast_loop (value socket, value v)
{
    CAMLparam2(socket, v);
    CAMLlocal1(result);
    
    int fd = Int_val(socket);
    u_char loop = Int_val(v);
    int r = setsockopt(fd, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop));

    result = Val_int(r);
    CAMLreturn(result);
}

value
ounix_join_multicast_group (value socket, value group_msw, value group_lsw)
{
    CAMLparam3(socket, group_msw, group_lsw);
    CAMLlocal1(result);
    
    int sd = Int_val(socket);
    in_addr_t group = (Int_val(group_msw) << 16) | Int_val(group_lsw);
    
    int r;
    struct ip_mreq mreq;

    mreq.imr_multiaddr.s_addr=htonl(group);
    mreq.imr_interface.s_addr=htonl(INADDR_ANY);
      
   if (!IN_MULTICAST(ntohl(mreq.imr_multiaddr.s_addr))) {
        /* XXX raise an exception here instead of exit - avsm */
        fprintf(stderr, "given address '%s' is not multicast\n",inet_ntoa(mreq.imr_multiaddr));
        exit(1);
   }
  
   r = setsockopt(sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *) &mreq, sizeof(mreq));

   result = Val_int(r);
   CAMLreturn(result);
}
