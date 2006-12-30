(*
 * Copyright (c) 2005 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2005 David Scott <djs@fraserresearch.org>
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
 
 * $Id: dig.ml,v 1.5 2005/12/05 00:46:07 avsm Exp $
 *)

open Unix
open Printf
open Udp
open Dns
open Dns_rr

module M = Mpl_stdlib
module ML = M.Mpl_dns_label
module DQ = Dns.Questions

let debug_active = ref false
let debug x = if !debug_active then prerr_endline (sprintf "[debug] %s" x)
let warn x y = prerr_endline (sprintf "%s warning: %s" x y)
let error x y = prerr_endline (sprintf "%s error: %s" x y); exit(1)

let got_alrm = ref false

let ip_of_uint32 s =
   let (>>) x y = Int32.logand (Int32.shift_right x y) 255l in
   sprintf "%ld.%ld.%ld.%ld" (s >> 24) (s >> 16) (s >> 8) (s >> 0) 

let send_query env fd qcl qty arg =
    M.reset env;
    ML.init_marshal env;
    let q = DQ.t ~qname:(Str.split (Str.regexp_string ".") arg) ~qtype:qty ~qclass:qcl in
    ignore (Dns.t ~id:0 ~qr:`Query ~opcode:`Query ~authoritative:0 ~truncation:0 ~rd:1
        ~ra:0 ~rcode:`NoError ~questions:[q] ~answers:[] ~additional:[] ~authority:[] env);
    (* XXX set udp checksum here *)
    M.flush env fd

let dump_answers env fd timeout arg =
    M.reset env;
    printf ";; <<>> MLDiG 0.1 <<>> %s\n" arg;
    printf ";; global options: \n";
    (* setup itimer to timeout *)
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> got_alrm := true));
    ignore (setitimer ITIMER_REAL {it_interval=timeout;it_value=0.});
    let _ = try M.fill env fd with Unix_error (e,_,_) -> begin
      if !got_alrm then 
        printf ";; connection timed out; no servers could be reached\n"
      else
        printf ";; read error: %s\n" (error_message e);
      exit 0;
    end in
    let uc = String.uppercase in let al = Array.length in
    ML.init_unmarshal env;
    let dns = Dns.unmarshal env in
    let os s = printf ";; %s SECTION:\n" (String.uppercase s) in
    let onl () = print_newline () in
    let bitfn a b = if a = 0 then None else Some b in
    let flags = [ (match dns#qr with |`Unknown _ -> None |`Query -> None |`Answer -> Some "qr"); (bitfn dns#authoritative "aa");
        (bitfn dns#truncation "tc"); (bitfn dns#rd "rd"); (bitfn dns#ra "ra"); ] in
    let flagstr = String.concat " " (List.fold_left (fun a -> function |None -> a |Some x -> x :: a) [] flags) in
    printf ";; ->>HEADER<<- opcode: %s, status: %s, id: %u\n" 
        (uc (Dns.opcode_to_string dns#opcode)) (uc (Dns.rcode_to_string dns#rcode)) dns#id;
    printf ";; flags: %s; QUERY: %d, ANSWER: %d, AUTHORITY: %d, ADDITIONAL: %d\n\n" flagstr
        (al dns#questions) (al dns#answers) (al dns#authority) (al dns#additional);
    if al dns#questions > 0 then begin
        os "question";
        Array.iter (fun (q:DQ.o) -> printf ";%-23s %-8s %-8s %s\n"
            (String.concat "." q#qname) "" (DQ.qclass_to_string q#qclass) (DQ.qtype_to_string q#qtype)
        ) dns#questions;
        onl ();
    end;
    let outfn a d e = printf "%-24s %-8lu %-8s %-8s %s\n" (String.concat "." a#name) a#ttl (Dns_rr.aclass_to_string a#aclass) d e in
    List.iter (fun (nm,ob) ->
      if al ob > 0 then os nm;
      Array.iter (fun x -> match ((x#rr):Dns_rr.o) with
      |`A a -> outfn a "A" (ip_of_uint32 a#ip);
      |`SOA a -> outfn a "SOA" (sprintf "%s %s %lu %lu %lu %lu %lu" (String.concat "." a#primary_ns)
        (String.concat "." a#admin_mb) a#serial a#refresh a#retry a#expiration a#minttl);
      |`MX a -> outfn a "MX" (sprintf "%d %s" a#preference (String.concat "." a#hostname));
      |`CNAME a -> outfn a "CNAME" (String.concat "." a#cname)
      |`NS a -> outfn a "NS" (String.concat "." a#hostname)
      |_ -> printf "unknown\n"
      ) ob;
      if al ob > 0 then onl ()
    ) ["answer",dns#answers; "authority",dns#authority; "additional",dns#additional]
   
let _ =
   let env = M.new_env (String.make 1000 '\000') in
    let lookupopts = Resolvconf.parse_file () in
    let server = ref (Resolvconf.choose_server lookupopts) in
    let dp = try (Unix.getservbyname "domain" "udp").Unix.s_port with Not_found -> 53 in
    let dest_port = ref dp in
    let args = ref [] in
    let qty = ref `A in
    let qcl = ref `IN in
    let timeout = ref 2 in
    let notimpl x = warn "dig" ("Command-line option \"" ^ x ^ "\" is not supported.") in
    let spec = [ "-b", Arg.String (fun x -> notimpl "-b"), "set source IP of query";
      "-c", Arg.String (fun x -> notimpl "-c"), "set query class (default IN)";
      "-f", Arg.String (fun x -> notimpl "-f"), "read batched requests from file";
      "-k", Arg.String (fun x -> notimpl "-k"), "sign outgoing queries";
      "-p", Arg.Int (fun x -> dest_port := x), (sprintf "set destination port number (default %d)" dp);
      "-t", Arg.String (fun x -> notimpl "-t"), "set query type (default to A unless -x is set)";
      "-x", Arg.String (fun x -> notimpl "-x"), "do reverse lookup";
      "-y", Arg.String (fun x -> notimpl "-y"), "key to sign request";
      "-v", Arg.Unit (fun () -> debug_active := true), "turn on debug output";
     ] in
    
    let anon_arg x = 
      if String.length x > 1 && (x.[0] = '@') then begin
        server := Some (String.sub x 1 (String.length x - 1))
      end else begin
        match DQ.qtype_of_string (String.uppercase x) with
        |None -> begin
          match DQ.qclass_of_string (String.uppercase x) with
          |None ->
            args := !args @ [x];
            debug (sprintf "Setting HOSTNAME = %s" x);
          |Some cl ->
            qcl := cl;
            debug (sprintf "Setting CLASS = %s" (Dns.Questions.qclass_to_string !qcl));
          end
        |Some ty -> begin
          qty := ty;
          debug (sprintf "Setting TYPE = %s" (Dns.Questions.qtype_to_string !qty));
        end
      end
    in
    Arg.parse spec anon_arg "generate and send DNS queries";
    if !args = [] then error "dig" "Must specify at least one hostname to resolve.";
    if List.length !args > 1 then error "dig" "Only one name can be resolved at a time.";
    match !server with
    |None -> error "dig" "Must specify a DNS resolver (with @<hostname>)"
    |Some x -> 
      let hinf = Unix.gethostbyname x in
      if Array.length hinf.Unix.h_addr_list = 0 then error "dig" (sprintf "Unable to resolve %s" x);
      debug (sprintf "Querying DNS server %s" x);
      let s = Unix.socket hinf.Unix.h_addrtype Unix.SOCK_DGRAM 0 in
      Unix.handle_unix_error Unix.connect s (Unix.ADDR_INET (hinf.Unix.h_addr_list.(0), 53));
      send_query env s !qcl !qty (List.hd !args);
      dump_answers env s (float_of_int !timeout) (List.hd !args)
