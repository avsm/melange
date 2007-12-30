(*
 * Copyright (c) 2005,2006 Anil Madhavapeddy <anil@recoil.org>
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
 *
 * $Id: ounix.ml,v 1.4 2006/03/17 02:56:43 avsm Exp $
 *)

open Unix

module Bindings = struct
  (* External C bindings; not to be exposed outside this file *)
  external set_tcp_nodelay : Unix.file_descr -> bool -> unit =
    "ounix_set_tcp_nodelay"
      
  external set_ip_multicast_ttl : Unix.file_descr -> int -> unit =
    "ounix_set_ip_multicast_ttl"
      
  external set_ip_multicast_loop : Unix.file_descr -> int -> unit =
    "ounix_set_ip_multicast_loop"
      
  external join_multicast_group : Unix.file_descr -> int -> int -> unit =
    "ounix_join_multicast_group"

  external daemon : int -> int -> unit =
	"ounix_daemon"
end      

(* Pseudo terminal handling functions *)
module Pty = struct

	type pty = {
	    masterfd: Unix.file_descr;
	    slavefd: Unix.file_descr;
	    name: string;
	}

	type pty_window = {
	    row: int32;
	    col: int32;
	    xpixel: int32;
	    ypixel: int32;
	}

	(* Exceptions raised by Pty functions *)
	exception Pty_error of string
	let _ = Callback.register_exception "pty_error" (Pty_error "")

	(* External declarations of Pty bindings *)
	external open_pty : unit -> pty = "pty_open_pty"
	external switch_controlling_pty : pty -> unit = "pty_switch_controlling_tty"
	external window_size : pty -> pty_window -> unit = "pty_window_size"

	(* Convenience ML functions *)
	let close_pty pty =
	    try Unix.close pty.masterfd with _ -> ();
	    try Unix.close pty.slavefd with _ -> ()

	(* Internal declarations of Pty bindings *)
	let string_of_pty p = Printf.sprintf "name=%s" p.name
end

(** Set or unset the TCP_NODELAY flag on a fd *)
let set_tcp_nodelay = Bindings.set_tcp_nodelay

(** Set the multicast TTL on an fd to x *)
let set_ip_multicast_ttl fd x = 
  Bindings.set_ip_multicast_ttl fd x

(** Set the IP_MULTICAST_LOOP on an fd to x *)
let set_ip_multicast_loop fd x = 
  Bindings.set_ip_multicast_loop fd x

(** Add the fd to the multicast group with address addr *)
let join_multicast_group fd addr = 
  (** Take an inet_addr and return an array of ints eg [| 127; 0; 0; 1 |] *)
  let octets_of_addr addr = 
    let s = Unix.string_of_inet_addr addr in
    let a = String.index_from s 0 '.' in
    let b = String.index_from s (a+1) '.' in
    let c = String.index_from s (b+1) '.' in
    Array.map int_of_string 
      [| String.sub s 0 a; 
	 String.sub s (a + 1) (b - a - 1);
	 String.sub s (b + 1) (c - b - 1); 
	 String.sub s (c + 1) (String.length s - c - 1) |] in
  (* Make into 16-bit words for transmission into the C bindings *)
  let octets = octets_of_addr addr in
  let msw = (octets.(0) lsl 8) lor octets.(1)
  and lsw = (octets.(2) lsl 8) lor octets.(3) in
  Bindings.join_multicast_group fd msw lsw

(* Daemonize process using daemon(3) *)
let daemon ?(chdir=true) ?(close=true) () =
	Bindings.daemon (if chdir then 0 else 1) (if close then 0 else 1)
	
let string_of_sockaddr = function
  |ADDR_UNIX (path) ->
    Printf.sprintf "\"%s\"" path
  |ADDR_INET (server_addr, port) ->
    Printf.sprintf "%s:%d" (string_of_inet_addr server_addr) port

class type odescr = object
    method fd : file_descr
    method read_ready : unit
    method write_ready : unit
    method exception_ready : unit
    method close: unit
end

class stream_odescr
    ?(rfn=(fun _ -> ()))
    ?(wfn=(fun _ -> ()))
    ?(efn=(fun _ -> ()))
    (fd:file_descr) = object(self)
    method fd = fd
    method read_ready = rfn (self :> stream_odescr)
    method write_ready = wfn (self :> stream_odescr)
    method exception_ready = efn (self :> stream_odescr)
   
    val mutable pos = 0
    method private atomicio fn str off len =
        pos <- 0;
        while len > pos do
            try
                let x = fn fd str (off + pos) (len - pos) in
                if x = 0 then raise (Unix_error (EUNKNOWNERR 0,"",""));
                pos <- pos + x;
            with 
            |Unix_error (EINTR,_,_)
            |Unix_error (EAGAIN,_,_) -> ()
        done
        
    method read = self#atomicio Unix.read
    method write = self#atomicio Unix.write

    method read_buf len =
        let buf = String.make len '\000' in
        self#read buf 0 len;
        buf

    method write_buf buf =
        self#write buf 0 (String.length buf)
        
    method close = close fd
end

class tcp_odescr ?rfn ?wfn ?efn fd = object(self)
    inherit stream_odescr ?rfn ?wfn ?efn fd
    method set_nodelay v = set_tcp_nodelay self#fd v
end

class dgram_odescr
    ?(rfn=(fun _ -> ()))
    ?(wfn=(fun _ -> ()))
    ?(efn=(fun _ -> ()))
    (fd:file_descr) = object(self)
    method fd = fd
    method read_ready = rfn (self :> dgram_odescr)
    method write_ready = wfn (self :> dgram_odescr)
    method exception_ready = efn (self :> dgram_odescr)
    
    method recvfrom = recvfrom fd
    method sendto = sendto fd
    
    method close = close fd
end

let udp_listener ?(interface=inet_addr_any) ?rfn ?wfn ?efn port =
    let s = socket PF_INET SOCK_DGRAM 0 in
    bind s (ADDR_INET (interface, port));
    new dgram_odescr ?rfn ?wfn ?efn s

let tcp_listener ?(interface=inet_addr_any) ?rfn ?wfn ?efn port =
    let s = socket PF_INET SOCK_STREAM 0 in
    bind s (ADDR_INET (interface, port));
    new tcp_odescr ?rfn ?wfn ?efn s

class oselect = object(self)
    val mutable ofds = []
    val mutable fdlist = []
    val fdcache:((file_descr,odescr) Hashtbl.t) = Hashtbl.create 1

    method add_ofd ofd =
        let fd = ofd#fd in
        ofds <- ofd :: ofds;
        fdlist <- fd :: fdlist;
        Hashtbl.add fdcache fd ofd
    
    method remove_ofd ofd =
        let fd = ofd#fd in
        ofds <- List.filter (fun x -> ofd <> x) ofds;
        fdlist <- List.filter (fun x -> fd <> x) fdlist;
        Hashtbl.remove fdcache fd

    method private lookup fd =
        try Hashtbl.find fdcache fd with
        Not_found -> failwith ("lookup failed in oselect")

    method read =
        let rfd,_,efd =
            try
                select fdlist [] fdlist (-1.0)
            with
            |Unix_error (x,_,_) as e ->
                match x with
                |EINTR -> ([],[],[])
                |_ -> raise e
        in
        List.iter (fun x -> (self#lookup x)#read_ready) rfd;
        if List.length efd > 0 then begin
            (* Cull any exceptional event descriptors from our list *)
            fdlist <- List.filter (fun x ->
                if List.mem x efd then begin
                    (self#lookup x)#exception_ready; false
                end else true) fdlist;
        end
end

class osignal = 
    let rec rep fn = function
     |0 -> ()
     |n -> fn (); rep fn (n-1) in
    object(self)
    val mutable sigchld = 0
    val mutable sigchldfns = []
    val mutable sighup = 0
    val mutable sighupfns = []
    
    initializer
        Sys.set_signal Sys.sigchld (Sys.Signal_handle 
            (fun _ -> sigchld <- succ sigchld));
        Sys.set_signal Sys.sighup (Sys.Signal_handle
            (fun _ -> sighup <- succ sighup))
   
    method add_sigchld_handler fn =
        sigchldfns <- fn :: sigchldfns
        
    method add_sighup_handler fn =
        sighupfns <- fn :: sighupfns
        
    method process =
        (* XXX block signals and all that stuff here - avsm *)
        rep (fun () ->
            List.iter (fun x -> x ()) sigchldfns;
            sigchld <- sigchld - 1) sigchld;
        rep (fun () ->
            List.iter (fun x -> x ()) sighupfns;
            sighup <- sighup - 1) sighup
        
end

