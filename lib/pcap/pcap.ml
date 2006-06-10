(*
 * Copyright (c) 2006 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: pcap.ml,v 1.1 2006/05/31 05:30:46 avsm Exp $
 *)

open Pcap_entry
open Printf

exception Parse_error of string

let parse_file dissectfn fname =
    let dbg = print_endline in
    let module M = Mpl_stdlib in
    let fin = open_in fname in
    let fillfn buf off len =
        really_input fin buf off len;
        len
    in
    let env = M.new_env ~fillfn:fillfn (String.make 60000 '\000') in
    let read16 () = M.Mpl_uint16.to_int (M.Mpl_uint16.unmarshal env) in
    let read32 () = M.Mpl_uint32.to_int32 (M.Mpl_uint32.unmarshal env) in
    let endian_fn = match read32 () with
    |0xa1b2c3d4l -> M.set_big_endian
    |0xd4c3b2a1l -> M.set_little_endian
    |magic -> raise (Parse_error (sprintf "Unknown pcap magic %lx" magic))
    in
    endian_fn ();
    let major_version = read16 () in
    let minor_version = read16 () in
    let tz_offset = read32 () in
    let time_accuracy = read32 () in
    let snaplen = read32 () in
    let lltype = read32 () in
    dbg (sprintf "V%d.%d; Offset %lu; Accuracy %lu; Snaplen %lu; Link %lu"
        major_version minor_version tz_offset time_accuracy snaplen lltype);
    let cont = ref true in
    let pos = ref (M.curpos env) in
    try while !cont do
        endian_fn ();
        let entenv = M.env_at env !pos 60000 in
        let ent = Pcap_entry.unmarshal entenv in
        pos := !pos + M.curpos entenv;
        let caplen = ent#data_length in
        let reallen = Int32.to_int ent#reallen in
        dbg (sprintf "Entry: %lu.%lu sec (%d/%d)" ent#sec ent#usec caplen reallen);
        let ipenv = ent#data_env in
        M.set_network_endian ();
        if caplen = 0 then cont := false else dissectfn ipenv
    done
    with End_of_file -> ()
