(*
 * Copyright (c) 2004 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: ssh_tty.ml,v 1.2 2006/01/14 19:24:19 avsm Exp $
 *)

(* Parse terminal modes as defined in draft-ietf-secsh-connect-21.txt *)
   
open Unix

module Tty = struct

    exception Unknown_terminal_entry
    exception Malformed_terminal_string
    
    type entry =
        |TTY_OP_END
        |VINTR
        |VQUIT
        |VERASE
        |VKILL
        |VEOF
        |VEOL
        |VEOL2
        |VSTART
        |VSTOP
        |VSUSP
        |VDSUSP
        |VREPRINT
        |VWERASE
        |VLNEXT
        |VFLUSH
        |VSWTCH
        |VSTATUS
        |VDISCARD
        |IGNPAR
        |PARMRK
        |INPCK
        |ISTRIP
        |INLCR
        |IGNCR
        |ICRNL
        |IUCLC
        |IXON
        |IXANY
        |IXOFF
        |IMAXBEL
        |ISIG
        |ICANON
        |XCASE
        |ECHO
        |ECHOE
        |ECHOK
        |ECHONL
        |NOFLSH
        |TOSTOP
        |IEXTEN
        |ECHOCTL
        |ECHOKE
        |PENDIN
        |OPOST
        |OLCUC
        |ONLCR
        |OCRNL
        |ONOCR
        |ONLRET
        |CS7
        |CS8
        |PARENB
        |PARODD
        |TTY_OP_ISPEED
        |TTY_OP_OSPEED

    let int_of_entry = function
        |TTY_OP_END -> 0
        |VINTR -> 1
        |VQUIT -> 2
        |VERASE -> 3
        |VKILL -> 4
        |VEOF -> 5
        |VEOL -> 6
        |VEOL2 -> 7
        |VSTART -> 8
        |VSTOP -> 9
        |VSUSP -> 10
        |VDSUSP -> 11
        |VREPRINT -> 12
        |VWERASE -> 13
        |VLNEXT -> 14
        |VFLUSH -> 15
        |VSWTCH -> 16
        |VSTATUS -> 17
        |VDISCARD -> 18
        |IGNPAR -> 30
        |PARMRK -> 31
        |INPCK -> 32
        |ISTRIP -> 33
        |INLCR -> 34
        |IGNCR -> 35
        |ICRNL -> 36
        |IUCLC -> 37
        |IXON -> 38
        |IXANY -> 39
        |IXOFF -> 40
        |IMAXBEL -> 41
        |ISIG -> 50
        |ICANON -> 51
        |XCASE -> 52
        |ECHO -> 53
        |ECHOE -> 54
        |ECHOK -> 55
        |ECHONL -> 56
        |NOFLSH -> 57
        |TOSTOP -> 58
        |IEXTEN -> 59
        |ECHOCTL -> 60
        |ECHOKE -> 61
        |PENDIN -> 62
        |OPOST -> 70
        |OLCUC -> 71
        |ONLCR -> 72
        |OCRNL -> 73
        |ONOCR -> 74
        |ONLRET -> 75
        |CS7 -> 90
        |CS8 -> 91
        |PARENB -> 92
        |PARODD -> 93
        |TTY_OP_ISPEED -> 128
        |TTY_OP_OSPEED -> 129

    let entry_of_int = function
        |0 -> TTY_OP_END
        |1 -> VINTR
        |2 -> VQUIT
        |3 -> VERASE
        |4 -> VKILL
        |5 -> VEOF
        |6 -> VEOL
        |7 -> VEOL2
        |8 -> VSTART
        |9 -> VSTOP
        |10 -> VSUSP
        |11 -> VDSUSP
        |12 -> VREPRINT
        |13 -> VWERASE
        |14 -> VLNEXT
        |15 -> VFLUSH
        |16 -> VSWTCH
        |17 -> VSTATUS
        |18 -> VDISCARD
        |30 -> IGNPAR
        |31 -> PARMRK
        |32 -> INPCK
        |33 -> ISTRIP
        |34 -> INLCR
        |35 -> IGNCR
        |36 -> ICRNL
        |37 -> IUCLC
        |38 -> IXON
        |39 -> IXANY
        |40 -> IXOFF
        |41 -> IMAXBEL
        |50 -> ISIG
        |51 -> ICANON
        |52 -> XCASE
        |53 -> ECHO
        |54 -> ECHOE
        |55 -> ECHOK
        |56 -> ECHONL
        |57 -> NOFLSH
        |58 -> TOSTOP
        |59 -> IEXTEN
        |60 -> ECHOCTL
        |61 -> ECHOKE
        |62 -> PENDIN
        |70 -> OPOST
        |71 -> OLCUC
        |72 -> ONLCR
        |73 -> OCRNL
        |74 -> ONOCR
        |75 -> ONLRET
        |90 -> CS7
        |91 -> CS8
        |92 -> PARENB
        |93 -> PARODD
        |128 -> TTY_OP_ISPEED
        |129 -> TTY_OP_OSPEED
        |_ -> raise Unknown_terminal_entry

    let string_of_entry = function
        |TTY_OP_END -> "TTY_OP_END"
        |VINTR -> "VINTR"
        |VQUIT -> "VQUIT"
        |VERASE -> "VERASE"
        |VKILL -> "VKILL"
        |VEOF -> "VEOF"
        |VEOL -> "VEOL"
        |VEOL2 -> "VEOL2"
        |VSTART -> "VSTART"
        |VSTOP -> "VSTOP"
        |VSUSP -> "VSUSP"
        |VDSUSP -> "VDSUSP"
        |VREPRINT -> "VREPRINT"
        |VWERASE -> "VWERASE"
        |VLNEXT -> "VLNEXT"
        |VFLUSH -> "VFLUSH"
        |VSWTCH -> "VSWTCH"
        |VSTATUS -> "VSTATUS"
        |VDISCARD -> "VDISCARD"
        |IGNPAR -> "IGNPAR"
        |PARMRK -> "PARMRK"
        |INPCK -> "INPCK"
        |ISTRIP -> "ISTRIP"
        |INLCR -> "INLCR"
        |IGNCR -> "IGNCR"
        |ICRNL -> "ICRNL"
        |IUCLC -> "IUCLC"
        |IXON -> "IXON"
        |IXANY -> "IXANY"
        |IXOFF -> "IXOFF"
        |IMAXBEL -> "IMAXBEL"
        |ISIG -> "ISIG"
        |ICANON -> "ICANON"
        |XCASE -> "XCASE"
        |ECHO -> "ECHO"
        |ECHOE -> "ECHOE"
        |ECHOK -> "ECHOK"
        |ECHONL -> "ECHONL"
        |NOFLSH -> "NOFLSH"
        |TOSTOP -> "TOSTOP"
        |IEXTEN -> "IEXTEN"
        |ECHOCTL -> "ECHOCTL"
        |ECHOKE -> "ECHOKE"
        |PENDIN -> "PENDIN"
        |OPOST -> "OPOST"
        |OLCUC -> "OLCUC"
        |ONLCR -> "ONLCR"
        |OCRNL -> "OCRNL"
        |ONOCR -> "ONOCR"
        |ONLRET -> "ONLRET"
        |CS7 -> "CS7"
        |CS8 -> "CS8"
        |PARENB -> "PARENB"
        |PARODD -> "PARODD"
        |TTY_OP_ISPEED -> "TTY_OP_ISPEED"
        |TTY_OP_OSPEED -> "TTY_OP_OSPEED"

    let set_mode tio v = function
        |VINTR -> if v != 255 then tio.c_vintr <- char_of_int v
        |VQUIT -> if v != 255 then tio.c_vquit <- char_of_int v
        |VERASE -> if v != 255 then tio.c_verase <- char_of_int v
        |VKILL -> if v != 255 then tio.c_vkill <- char_of_int v
        |VEOF -> if v != 255 then tio.c_veof <- char_of_int v
        |VEOL -> if v != 255 then tio.c_veol <- char_of_int v
        |VEOL2 -> ()
        |VSTART -> if v != 255 then tio.c_vstart <- char_of_int v
        |VSTOP -> if v != 255 then tio.c_vstop <- char_of_int v
        |VSUSP -> ()
        |VDSUSP -> ()
        |VREPRINT -> ()
        |VWERASE -> ()
        |VLNEXT -> ()
        |VFLUSH -> ()
        |VSWTCH -> ()
        |VSTATUS -> ()
        |VDISCARD -> ()
        |IGNPAR -> tio.c_ignpar <- (v != 0)
        |PARMRK -> tio.c_parmrk <- (v != 0)
        |INPCK -> tio.c_inpck <- (v != 0)
        |ISTRIP -> tio.c_istrip <- (v != 0)
        |INLCR -> tio.c_inlcr <- (v != 0)
        |IGNCR -> tio.c_igncr <- (v != 0)
        |ICRNL -> tio.c_icrnl <- (v != 0)
        |IUCLC -> ()
        |IXON -> tio.c_ixon <- (v != 0)
        |IXANY -> ()
        |IXOFF -> tio.c_ixoff <- (v != 0)
        |IMAXBEL -> ()
        |ISIG -> tio.c_isig <- (v != 0)
        |ICANON -> tio.c_icanon <- (v != 0)
        |XCASE -> ()
        |ECHO -> tio.c_echo <- (v != 0)
        |ECHOE -> tio.c_echoe <- (v != 0)
        |ECHOK -> tio.c_echok <- (v != 0)
        |ECHONL -> tio.c_echonl <- (v != 0)
        |NOFLSH -> tio.c_noflsh <- (v != 0)
        |TOSTOP -> ()
        |IEXTEN -> ()
        |ECHOCTL -> ()
        |ECHOKE -> ()
        |PENDIN -> ()
        |OPOST -> tio.c_opost <- (v != 0)
        |OLCUC -> ()
        |ONLCR -> ()
        |OCRNL -> ()
        |ONOCR -> ()
        |ONLRET -> ()
        |CS7 -> tio.c_csize <- 7
        |CS8 -> tio.c_csize <- 8
        |PARENB -> tio.c_parenb <- (v != 0)
        |PARODD -> tio.c_parodd <- (v != 0)
        |TTY_OP_ISPEED -> tio.c_ibaud <- v
        |TTY_OP_OSPEED -> tio.c_obaud <- v
        |TTY_OP_END -> ()

    (* Parse the mode string x and mutate the termios stored in tio.
       Raised Malformed_terminal_string if the string is bogus *)
    let parse_modes (tio:terminal_io) (x:string) =
        ()
        (* XXX rewrite using Mpl_env, this is just another packet format *)
        (* 
        let op_end = ref false in
        let off = ref 0 in
        let get_byte x =
            let x' = Char.code (String.get x !off) in
            incr off;
            x' in
        while not !op_end do
            let op = try get_byte x with _ -> raise Malformed_terminal_string in
            try match entry_of_int op with
            |TTY_OP_END ->
                op_end := true
            |e ->
                let vl, o = Unmarshal.int x (!off) in
                set_mode tio vl e;
                off := o
            with Unknown_terminal_entry -> begin
                print_endline "unknown terminal entry";
                op_end := true;
            end;
        done;
        ()
        *)
        
end
