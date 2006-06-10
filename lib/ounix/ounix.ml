(*
 * Copyright (c) 2005,2006 Anil Madhavapeddy <anil@recoil.org>
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

(* External C bindings *)
external set_tcp_nodelay : Unix.file_descr -> bool -> unit =
    "ounix_set_tcp_nodelay"

external set_ip_multicast_ttl : Unix.file_descr -> int -> int =
    "ounix_set_ip_multicast_ttl"

external set_ip_multicast_loop : Unix.file_descr -> int -> int =
    "ounix_set_ip_multicast_loop"

external join_multicast_group : Unix.file_descr -> int -> int -> int =
    "ounix_join_multicast_group"

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
        
    method in_channel = in_channel_of_descr fd
    method out_channel = out_channel_of_descr fd
    
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
