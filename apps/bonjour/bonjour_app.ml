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

(** Temporary function which sends a query up to 'maxtries' tries at 
    intervals of 'interval'. We probably need something more sophisticated
    able to handle (i) multiple requests; (ii) different backoff strategies *)
let do_query s send_query_fn receive_fn maxtries interval maxwait = 
    let start = Unix.gettimeofday () in
    let rec loop attempts_left time_left = match attempts_left, time_left with
    | 0, _ -> []
    | _, x when x <= 0. -> []
    | _, _ ->
        send_query_fn s;
        let r, _, _ = Unix.select [ s ] [ ] [ ] (min interval time_left) in
        let time_left = start +. maxwait -. (Unix.gettimeofday ()) in
        if r = [] 
        then loop (attempts_left - 1) time_left
        else receive_fn s :: (loop (attempts_left - 1) time_left) in
    loop maxtries maxwait

module M = Mpl_stdlib
open Dns

let _ = 
    let senv = M.new_env (String.make 4000 '\000') in
    let env = M.new_env (String.make 4000 '\000') in

    let send_query q s = 
        print_endline "Sending query...";
        M.reset senv;
        M.Mpl_dns_label.init_marshal senv;
        ignore(Bonjour.simple_lookup `PTR q senv);
        M.env_send_fn senv (fun buf off len -> 
            Printf.printf "sending %d bytes\n" len;
            let len = Unix.sendto s buf off len [] Bonjour.addr in
            Printf.printf "  returned %d\n" len) in
         
    let receive_response s = 
        print_endline "Receiving response...\n";
        M.reset env;
        M.Mpl_dns_label.init_unmarshal env;
        M.env_recv_fn env (fun buf off len -> Unix.recvfrom s buf off len []);
        Dns.unmarshal env in

    let s = Bonjour.connect () in
    let x = do_query s (send_query Bonjour.all_services) receive_response 10 1. 10. in
    List.iter (fun x -> x#prettyprint) x
        