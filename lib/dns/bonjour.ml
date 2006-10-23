(*
 * Copyright (c) 2006 David Scott <dave@recoil.org>
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
 
 * $Id:$
 *)

(** Utility functions to handle multicast DNS and Bonjour *)

let ip = "224.0.0.251"
let port = 5353
let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)

(** Return a socket connected to the mDNS multicast group *)
let connect ?(port=0) () = 
  (* Bind a local socket *)
  let s = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.handle_unix_error (Unix.bind s) (Unix.ADDR_INET (Unix.inet_addr_any, port));
  (* Join the socket to the multicast group *)
  Ounix.set_ip_multicast_ttl s 1;
  Ounix.set_ip_multicast_loop s 1;
  Ounix.join_multicast_group s (Unix.inet_addr_of_string ip);
  s

(** Query which returns all services on the network *)
let all_services = [ "_services"; "_dns-sd"; "_udp"; "local" ]

let next_transaction_id = 
    let id = ref 0 in
    fun () ->
        let result = !id in
        id := !id + 1;
        result mod 65536

open Dns
open Dns_rr

let simple_lookup qtype qname = 
    let qs = [Dns.Questions.t ~qname:qname ~qtype:qtype ~qclass:`IN] in

    Dns.t ~id:(next_transaction_id())
          ~qr:`Query ~opcode:`Query ~authoritative:0 ~rcode:`NoError
          ~truncation:0 ~rd:1 ~ra:0 
          ~questions:qs ~answers:[] ~authority:[] ~additional:[]

let a_record ?(cls=`IN) hostname ip = 
    Dns_rr.A.t 
	~name:hostname 
	~aclass:cls
	~ttl:120l (* seconds, default for hostnames *)
	~ip:ip 

(** A probe is a question containing the proposed answer. This is the 
    probe for an A record  *)
let probe_ptr hostname ip = 
    let qs = [Dns.Questions.t ~qname:hostname ~qtype:`A ~qclass:`IN] in
    let answers = [Dns.Answers.t ~rr:(`A (a_record hostname ip))] in
    Dns.t ~id:(next_transaction_id())
          ~qr:`Query ~opcode:`Query ~authoritative:0 ~rcode:`NoError
          ~truncation:0 ~rd:0 ~ra:0 
          ~questions:qs ~answers:answers ~authority:[] ~additional:[]

let response_ptr hostname ip req = 
    let answers = [Dns.Answers.t ~rr:(`A (a_record ~cls:(`Unknown 0x8001) hostname ip))] in
     Dns.t ~id:0
          ~qr:`Answer ~opcode:`Query ~authoritative:1 ~rcode:`NoError
          ~truncation:0 ~rd:0 ~ra:0 
          ~questions:[] ~answers:answers ~authority:[] ~additional:[]
   
