(* Demonstration of the Getopt module *)

open Getopt

let archive = ref false
and update  = ref false
and verbose = ref 0
and includ  = ref []
and output  = ref ""

let bip ()  = Printf.printf "\007"; flush stdout
let wait () = Unix.sleep 1 

let specs = 
[
  ( 'x', "execute", None, Some (fun x -> Printf.printf "execute %s\n" x));
  ( 'I', "include", None, (append includ));
  ( 'o', "output",  None, (atmost_once output (Error "only one output")));
  ( 'a', "archive", (set archive true), None);
  ( 'u', "update",  (set update  true), None);
  ( 'v', "verbose", (incr verbose), None);
  ( 'X', "",        Some bip, None);
  ( 'w', "wait",    Some wait, None)

]

let _ = 
  parse_cmdline specs print_endline;

  Printf.printf "archive = %b\n" !archive;
  Printf.printf "update  = %b\n" !update;
  Printf.printf "verbose = %i\n" !verbose;
  Printf.printf "output  = %s\n" !output;
  List.iter (fun x -> Printf.printf "include %s\n" x) !includ;;
