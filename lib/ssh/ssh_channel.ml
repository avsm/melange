(*
 * Copyright (c) 2004,2005 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: ssh_channel.ml,v 1.18 2006/03/16 12:23:01 avsm Exp $
 *)

open Ssh_utils

exception Bad_access

class channel
    ~(packet_size:int32)
    ~(initial_window:int32)
    ~(channel:int32) =
    let chan_access = function
    |Some x -> x |None -> raise Bad_access in object(self)
    val mutable close = false
    val mutable interactive = false
    val mutable pty = None

    val mutable automaton =
        let auto = Ssh_server_channel.init () in
        let sf = Spl_stdlib.get_tcp_sock_factory 1235 in
        let oc,_ = sf () in
        Ssh_server_channel.pagefn oc;
        Ssh_server_channel.set_cfn auto sf;
        auto
   
    method automaton = automaton
    method tick_automaton (s:Ssh_statecalls.t) =
        automaton <- Ssh_server_channel.tick automaton s
    method tick_automaton (s:Ssh_statecalls.t) = ()
    
    val our_channel = channel
    val our_packet_size = packet_size
    val our_initial_window = initial_window
    val mutable our_window = initial_window

    val mutable other_channel = (None:int32 option)
    method other_id = chan_access other_channel
    method set_other_id w = other_channel <- Some w

    val mutable other_initial_window = (None:int32 option)
    method other_initial_window = chan_access other_initial_window
    method set_other_initial_window w = other_initial_window <- Some w
    
    val mutable other_window = 0l
    method other_window = other_window
    
    val mutable other_packet_size = (None:int32 option)
    method other_packet_size = chan_access other_packet_size
    method set_other_packet_size w = other_packet_size <- Some w

    val mutable stdin = None
    val mutable stdout = None
    val mutable stderr = None
    val mutable pid = None
    val mutable exit_status = None
    
    method set_exit_status (x:int) = exit_status <- Some x
    method exit_status = exit_status
    
    (* Register a pty, also makes channel interactive *)
    method set_pty ((pt,pw): (Ssh_pty.pty * Ssh_pty.pty_window)) =
        pty <- Some (pt,pw);
        interactive <- true
    method close_pty =
        umay (fun (pt,_) -> Ssh_pty.close_pty pt) pty
       
    method set_pid (po:int) = pid <- Some po
    method set_close (x:bool) = close <- x

    method clear_pid = pid <- None

    method private opt_close (a:Ounix.stream_odescr option) = match a with
        |None -> () |Some x -> (try Unix.close x#fd with _ -> ())

    method set_stdin (fdo:Ounix.stream_odescr option) =
        self#opt_close stdin; stdin <- fdo
    method set_stdout (fdo:Ounix.stream_odescr option) =
        self#opt_close stdout; stdout <- fdo
    method set_stderr (fdo:Ounix.stream_odescr option) =
        self#opt_close stderr; stderr <- fdo

    method pty = pty
    method our_id = our_channel
    method our_initial_window = our_initial_window
    method our_window = our_window
    method set_our_window w = our_window <- w
    method set_other_window w = other_window <- w
    method our_packet_size = our_packet_size
    method stdin = stdin
    method stderr = stderr
    method stdout = stdout
    method pid = pid
    method close = close
    
    method consume_our_window datalen =
        (* XXX overflow checks *)
        let window_size = Int32.sub self#our_window datalen in
        let half_window = Int32.to_int our_initial_window / 2 in
        if (Int32.to_int window_size) <= half_window then begin
            our_window <- our_initial_window;
            Some (Int32.sub our_initial_window window_size);
        end else begin
            our_window <- window_size;
            None;
        end
end

type pty_req = {
    term: string;
    row: int;
    col: int;
    xpixel: int;
    ypixel: int;
    modes: string;
}

type halfchan = {
    chan: channel;
    pty: pty_req option;
    exec: string option;
    mutable confirm: (bool option * bool option);
}

exception Stop_iter
class channel_env = object(self)
    val mutable max_channel = 0l
    val chans = Hashtbl.create 1
    val halfchans = Hashtbl.create 1
    
    method private new_id = 
        let x = Int32.succ max_channel in
        max_channel <- x;
        x
        
    method new_chan ~(packet_size:int32) ~(initial_window:int32) =
        let our_id = self#new_id in
        let chan = new channel ~packet_size:packet_size
            ~initial_window:initial_window ~channel:our_id in
        Hashtbl.add chans our_id chan;
        Some chan

    method new_half_chan ~initial_window ~packet_size ~pty ~cmd =
        let our_id = self#new_id in
        let chan = new channel ~packet_size:packet_size
            ~initial_window:initial_window ~channel:our_id in
        let c = { chan=chan; pty=pty; exec=cmd; confirm=(None,None) } in
        Hashtbl.add halfchans our_id c;
        Some chan
   
    method progress_halfchans (chan:channel) v =
        try
            let x = Hashtbl.find halfchans chan#our_id in
            let () = match x.confirm with
            |(None,None) -> begin
                (* If we requested pty confirm it, else confirm exec *)
                match x.pty with
                |None -> x.confirm <- (None, Some v)
                |Some _ -> x.confirm <- (Some v,None)
              end
            |(Some a,None) -> x.confirm <- (Some a,Some v)
            |_ -> () in
            match x.confirm with
            |(a,Some b) ->
                (* Promote to full channel *)
                Hashtbl.add chans x.chan#our_id x.chan;
                Hashtbl.remove halfchans x.chan#our_id;
                Some (x.chan, a, b)
            |_ -> None
        with |Not_found -> None
            
    method find_halfchan_by_id id =
        try Some (Hashtbl.find halfchans id)
        with Not_found -> None

    method del_chan id =
        Hashtbl.remove chans id

    method find_by_id id =
        try Some (Hashtbl.find chans id)
        with Not_found -> None

    method find_by_pid pid =
        let found_pid = ref None in
        Hashtbl.iter (fun k v -> match v#pid with |None -> ()
            |Some p -> if pid = p then found_pid := Some v) chans;
        !found_pid

end
