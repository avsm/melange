
open Printf
open Config_t

(* We expect the test to either pass or fail at a particular line *)
type result =
	|Pass
	|Fail of int
	
let regress_files =
    let blank_fun = (fun (_:Config.config) -> ()) in
    let def = {t_short=None; t_long=None; t_name="";
        t_atom=T_unknown; t_descr=None; t_default=None} in [
    ("strings.conf", Pass,
	  [
		{def with t_short=Some 's'; t_name="config1"; t_atom=T_string; t_descr=None; t_default=Some (V_string "foo")};
		{def with t_name="config2"; t_atom=T_string; t_descr=None; t_default=None};
		{def with t_name="config3"; t_atom=T_string; t_descr=None; t_default=None};
		{def with t_name="config4.blah"; t_atom=T_string; t_descr=None; t_default=None};
		{def with t_name="config4.blah2"; t_atom=T_string; t_descr=None; t_default=None};
  	  ],
         (fun conf ->
            print_endline (sprintf "POST: strings.conf, config1= %s" (string_of_var_val (conf#get_val "config1")))
         )
        );
    ("variants.conf", Pass,
	  [
		{def with t_name="config1"; t_atom=(T_variant ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
		{def with t_name="config2"; t_atom=(T_variant ["Bar";"Foo"]); t_descr=None; t_default=None};
		{def with t_name="config3"; t_atom=(T_variant_list ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
		{def with t_name="config4"; t_atom=(T_variant_list ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
	  ], blank_fun
	);
    ("variants.conf", (Fail 3),
	  [
		{def with t_name="config1"; t_atom=(T_variant ["Bar";"Alpha"]); t_descr=None; t_default=None};
	  ], blank_fun
	);
    ("ip.conf", Pass,
	  [
		{def with t_name="config1"; t_atom=T_ip; t_descr=None; t_default=None};
		{def with t_name="config2"; t_atom=T_ip; t_descr=None; t_default=None};
		{def with t_name="config3"; t_atom=T_ip_list; t_descr=None; t_default=Some (V_ip (Unix.inet_addr_of_string "5.6.7.7"))};
		{def with t_name="config4"; t_atom=T_ip_list; t_descr=None; t_default=None};
		{def with t_name="config5"; t_atom=T_ip_list; t_descr=None; t_default=None};
 	  ], blank_fun
    );
]

(* ex=expected result, ac=actual result *)
let result file ex ac =
	let ok () = printf "===> OK: %s\n%!" file in
	let no x = printf "===> FAIL: %s: (%s)\n%!" file x in
	match ex,ac with
	|Pass,Pass -> ok ()
	|Fail x,Fail y ->
		if x = y then ok () else
			no (sprintf "Expected fail at %d, actually failed at %d" x y)
	|Pass,Fail x -> no (sprintf "Expected pass, actually failed at %d" x)
	|Fail x,Pass -> no (sprintf "Expected fail at %d, actually passed" x)
		
let _ =
    (* If REGRESS_CMDLINE is set, then only parse strings.conf *)
    let regress_files = try
        ignore(Sys.getenv("REGRESS_CMDLINE"));
        [List.hd regress_files]
      with
        Not_found -> regress_files
    in
    List.iter (fun (file,res,ty,postfn) ->
        printf "===> Testing: %s\n" file;
		try
		let v = new Config.config ty file in
		v#dump;
		result file res Pass;
                postfn v;
		with
		Config.Error (l,s) -> print_endline s; result file res (Fail l)
    ) regress_files
