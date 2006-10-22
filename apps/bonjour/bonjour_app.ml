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

let debug x = output_string stderr (x ^ "\n"); flush stderr

exception Parse_failure

(** Convert a dotted DNS name into a string list
    eg "www.example.com" -> [ "www"; "example"; "com" ] *)
let rec dotted_name_of_string x = 
  try
    let i = String.index x '.' in
    let left = String.sub x 0 i
    and right = String.sub x (i+1) (String.length x - i - 1) in
    left :: (dotted_name_of_string right)
  with Not_found -> [ x ]

let dns_name_of_string = dotted_name_of_string

let string_ip_of_uint32 s =
   let (>>) x y = Int32.logand (Int32.shift_right x y) 255l in
   Printf.sprintf "%ld.%ld.%ld.%ld" (s >> 24) (s >> 16) (s >> 8) (s >> 0) 

let uint32_of_string_ip x = 
    let (<<) = Int32.shift_left and (||) = Int32.logor in
    match List.map Int32.of_string (dns_name_of_string x) with
  | [a; b; c; d] -> (a << 24) || (b << 16) || (c << 8) || d
  | _ -> raise Parse_failure

(** Return a list of all packets received over the time 'interval' *)
let receive_all_for s receive_fn interval = 
    let start = Unix.gettimeofday () in
    let rec loop () = 
	let time_left = start +. interval -. (Unix.gettimeofday ()) in
	if time_left < 0. 
	then []
	else begin
	     let r, _, _ = Unix.select [ s ] [ ] [ ] time_left in
	     if r = [] then []
	     else let pkt = receive_fn s in
	     pkt :: (loop ())
	end in
    loop ()

exception Conflict of Dns.Dns.o list

(** Classify received packets into conflicts, requests for our data and
    others *)
let classify conflict_fn isrequest_fn all = 
    let conflicts, rest = List.partition conflict_fn all in
    let requests, rest  = List.partition isrequest_fn rest in
    debug(Printf.sprintf "   ... %d packets received; %d conflicts, %d requests, %d other"
            (List.length all) (List.length conflicts) 
            (List.length requests) (List.length rest));
    if conflicts <> [] then raise (Conflict conflicts);
    requests, rest

(** The initial probe phase involves:
    (1) an initial random delay between 0 and 250ms
    (2) three 'probe' queries transmitted (a probe is a question + answer)
	with intervals of 250ms
    (3) any responses have their RRs compared lexicographically; bigger
	wins, loser has to reconfigure *)
let do_probe s send_fn receive_fn conflict_fn =
    let interval = 0.25 in             (* 250ms inter-packet delay *)
    let initial_random_delay = 0.25 in (* 0 - 250ms random initial delay *) 
    Thread.delay initial_random_delay;
    for attempt = 1 to 3 do
	debug(Printf.sprintf "Sending probe %d/3 (%s)" attempt 
			     (if attempt = 3 then "QM" else "QU"));
	send_fn s;
	let all = receive_all_for s receive_fn interval in
	debug "classifying";
	let _, _ = classify conflict_fn (fun _ -> false) all in
	Thread.delay interval;
    done;
    debug(Printf.sprintf "No conflicts determined during probe phase")

(** The subsequent announcement phase involves:
    (1) initial gratuituous response multicast (delays 1s,1s... doubling
        after up to 8 responses) *)
let do_announce s send_fn receive_fn isrequest_fn conflict_fn = 
    (* send some gratuitous responses *)
    List.iter (fun delay ->
        send_fn s;
	let all = receive_all_for s receive_fn delay in
	(* Just ignore requests for now since we're sending gratuituous multicasts *)
	ignore(classify conflict_fn isrequest_fn all) 
    ) [ 1.; 1. ]

(** Main record servicing loop *)
let do_service_requests s send_fn receive_fn isrequest_fn conflict_fn = 
    debug("Waiting for requests");
    while true do
	  let r, _, _ = Unix.select [ s ] [ ] [ ] (-1.) in
          match classify conflict_fn isrequest_fn [ receive_fn s ] with
          | [ req ], _ ->
	    debug("Received request; multicasting response");
            send_fn s
          | _, _ -> ()
    done


(** For the special case of an A record determine whether the received
    'answer' conflicts with us (has the same name) and is lexicographically
    bigger (and so wins the fight) *)
let conflict our_hostname our_ip their_hostname their_ip = 
    our_hostname = their_hostname && (their_ip > our_ip)

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

(** Bonjour query to list all the services running on a network *)
let all_services = Bonjour.simple_lookup `PTR Bonjour.all_services

(** Program is single threaded; use static packet buffers for simplicity *)
let send_env = M.new_env (String.make 4000 '\000') 
let recv_env = M.new_env (String.make 4000 '\000') 

(** Helper function to send a query out a socket via the send_env *)
let send_query q s = 
  M.reset send_env;
  M.Mpl_dns_label.init_marshal send_env;
  ignore(q send_env);
  M.env_send_fn send_env (fun buf off len -> 
			    let len = Unix.sendto s buf off len [] Bonjour.addr in
			    ())
    
(** Helper function to receive a response from a socket via the recv_env *)
let receive_response s = 
  M.reset recv_env;
  M.Mpl_dns_label.init_unmarshal recv_env;
  M.env_recv_fn recv_env (fun buf off len -> Unix.recvfrom s buf off len []);
  Dns.unmarshal recv_env 

(** A set of DNS names (string lists) *)
module NameSet = Set.Make(struct type t = string list let compare = Pervasives.compare end)

(** Generate set of all available service names *)
let fetch_all_services s : NameSet.t = 
  let x = do_query s (send_query all_services) receive_response 10 1. 10. in
  (* Take each of the answers *)
  let answers = List.concat (List.map (fun x -> Array.to_list x#answers) x) in
  (* Extract the RRs *)
  let rrs = List.map (fun x -> x#rr) answers in
  (* Extract the PTR RRs *)
  let names = List.map (function `PTR x -> Some x#ptrdname | _ -> None ) rrs in
  List.fold_left (fun set elt -> match elt with
		  | Some x -> NameSet.add x set | None -> set) NameSet.empty names

(** An individual service is described by one of these: *)
type service_location = {
    host: string list;
    port: int;
    txt: string list
}

(** A set of service_location records *)
module SLSet = Set.Make(struct type t = service_location let compare = Pervasives.compare end)

(** Send a query for one particular service, return a set of service_location records *)
let find_one_service s name = 
  let query = Bonjour.simple_lookup `PTR name in
  let x = do_query s (send_query query) receive_response 10 1. 10. in
  let sls = List.map (fun response -> 
	       (* The hostname is a RR in the ANSWER section *)
	       let host = match response#answers with
		 | [| answer |] -> begin match answer#rr with
		   | `PTR ptr -> ptr#ptrdname
		   | _ -> []
		   end
		 | _ -> [] in
	       (* SRV and TXT RRs are in the additionals *)
	       let rrs = List.map (fun x -> x#rr) (Array.to_list response#additional) in
	       let port = match (List.filter (function `SRV _ -> true | _ -> false) rrs) with
		 | `SRV srv :: _ -> srv#port
		 | _ -> 0 in
	       let txt = match (List.filter (function `TXT _ -> true | _ -> false) rrs) with
		 | `TXT txt :: _ -> txt#data
		 | _ -> "" in
	       { host = host; port = port; txt = [ txt ]  }
	   ) x in
  List.fold_left (fun set elt -> SLSet.add elt set) SLSet.empty sls


let _ = 
  let mode = ref `AllServices in
  let string_of_mode = function
    | `AllServices -> "list all services"
    | `Single _ -> "query single service"
    | `Register _ -> "register and defend a RR" in
  let default = string_of_mode !mode in
  Arg.parse [ 
    "-all", Arg.Unit (fun _ -> mode := `AllServices),
    "list all network services";
    "-query", Arg.String (fun x -> mode := `Single x),
    "send queries for a single service (eg _ssh._tcp.local)";		
    "-defend", Arg.String (fun x -> mode := `Register x),
    "register and defend a RR"; 
  ]
    (fun x -> output_string stderr ("Ignoring argument: " ^ x))
    (Printf.sprintf "generate mDNS/Bonjour queries; default mode: %s (see -h)" default);

  let s = Bonjour.connect () in match !mode with
  | `AllServices ->  
    let all = fetch_all_services s in
    print_endline "List of unique service names found on the network:";
    NameSet.iter (fun name -> print_endline (String.concat "." name)) all
  | `Single x ->
    let all = find_one_service s (dns_name_of_string x) in
    print_endline ("Results for " ^ x);
    SLSet.iter (fun x -> 
		  print_endline ("  " ^ (String.concat "." x.host) ^ " : " ^ (string_of_int x.port));
		  List.iter (fun x -> print_endline ("    " ^ x)) x.txt) all
  | `Register x ->
    let us = dns_name_of_string x in
    let localhost = uint32_of_string_ip "127.0.0.1" in
    print_endline "Attempting to register and defend RR:";
    Printf.printf "%s  120  IN A  %s\n" x (string_ip_of_uint32 localhost);
    print_endline "1. Sending initial probes";
    let probe = Bonjour.probe_ptr us localhost in
    let any f xs = Array.fold_left (||) false (Array.map f xs) in
    let any_rr f xs = Array.fold_left (||) false (Array.map (fun x -> f x#rr) xs) in
    let conflict_fn response = 
	Array.fold_left (||) false 
	    (Array.map (fun x -> match x#rr with
		       | `A a -> conflict us localhost a#name a#ip
                       | _ -> debug("Ignoring spurious response");false) 
		       response#answers) in 
    let conflict_fn response = any_rr
	(function `A a -> conflict us localhost a#name a#ip
         | _ -> debug("Ignoring spurious response");false) response#answers in 
    let isrequest_fn request = any 
        (function x -> x#qname = us | _ -> false)
	(request#questions) in

    do_probe s (send_query probe) receive_response conflict_fn;
    print_endline "   Probing successful";
    print_endline "2. Entering announcement phase";
    do_announce s (send_query probe) receive_response isrequest_fn conflict_fn;
    print_endline "3. Entering service mode";
    do_service_requests s (send_query probe) receive_response isrequest_fn conflict_fn
 
