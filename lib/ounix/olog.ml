(*pp cpp *)
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
 *)

open Printf

type level = |Debug |Warning |Info |Critical

class virtual base_log = object(self)
    method virtual debug : string -> unit
    method virtual warning : string -> unit
    method virtual info  : string -> unit
    method virtual critical : string -> unit
    method virtual init : unit
    
    val mutable dl = Debug
    method private set_level x = dl <- x
    method set_debug = self#set_level Debug
    method set_warning = self#set_level Warning
    method set_info = self#set_level Info
    method set_critical = self#set_level Critical

    method debug_active = match dl with
        |Debug -> true |Warning|Info|Critical -> false
    method warning_active = match dl with
        |Debug|Warning -> true |Info|Critical -> false
    method info_active = match dl with
        |Debug|Warning|Info -> true |Critical -> false
    method critical_active = true
end

class stderr_log =
    let outputfn x = prerr_endline x in
    object(self)
    inherit base_log as base
    method init = self#info "started stderr_log"
    
    method debug x =
        outputfn (sprintf "{debug} %s" x)
    method warning x =
        outputfn (sprintf "{warning} %s" x)
    method info x =
        outputfn (sprintf "{info} %s" x)
    method critical x =
        outputfn (sprintf "{critical} %s" x)
end

class null_log = object(self)
    inherit base_log
    method init = ()
    method debug x = ()
    method warning x = ()
    method info x = ()
    method critical x = ()
end

