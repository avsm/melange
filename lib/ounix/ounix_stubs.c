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

/* From ocaml-src/otherlibs/unix/unixsupport.h */
#define Nothing ((value) 0)
extern void uerror (char * cmdname, value arg) Noreturn;

CAMLprim value
ounix_set_tcp_nodelay (value socket, value o)
{
    int opt = (Bool_val(o)) ? 1 : 0;
    if (setsockopt(Int_val(socket), IPPROTO_TCP, TCP_NODELAY, (void *)&opt, sizeof(opt)) == -1)
		uerror("setsockopt", Nothing);
	return Val_unit;
}

CAMLprim value
ounix_set_ip_multicast_ttl (value socket, value ttl)
{
    unsigned char t = Int_val(ttl);
    if (setsockopt(Int_val(socket), IPPROTO_IP, IP_MULTICAST_TTL, &t, sizeof(t)) == -1)
		uerror("setsockopt", Nothing);
	return Val_unit;
}

CAMLprim value
ounix_set_ip_multicast_loop (value socket, value v)
{
    unsigned char loop = Int_val(v);
    if (setsockopt(Int_val(socket), IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop)) == -1)
		uerror("setsockopt", Nothing);
	return Val_unit;
}

CAMLprim value
ounix_join_multicast_group (value socket, value group_msw, value group_lsw)
{
    in_addr_t group = (Int_val(group_msw) << 16) | Int_val(group_lsw);
    
    int r;
    struct ip_mreq mreq;

    mreq.imr_multiaddr.s_addr=htonl(group);
    mreq.imr_interface.s_addr=htonl(INADDR_ANY);
      
   if (!IN_MULTICAST(ntohl(mreq.imr_multiaddr.s_addr)))
		failwith("address is not multicast");
  
   if (setsockopt(Int_val(socket), IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *) &mreq, sizeof(mreq)) == -1)
		uerror("setsockopt", Nothing);
		
   return Val_unit;
}

CAMLprim value
ounix_daemon (value nochdir, value noclose)
{
	if (daemon(Int_val(nochdir), Int_val(noclose)) == -1)
		uerror("daemon", Nothing);
	return Val_unit;
}
