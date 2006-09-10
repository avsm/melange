
open Printf
open Config_t

(* We expect the test to either pass or fail at a particular line *)
type result =
	|Pass
	|Fail of int
	
let regress_files = [
	("strings.conf", Pass,
	  [
		{t_name="config1"; t_atom=T_string; t_descr=None; t_default=None};
		{t_name="config2"; t_atom=T_string; t_descr=None; t_default=None};
		{t_name="config3"; t_atom=T_string; t_descr=None; t_default=None};
		{t_name="config4.blah"; t_atom=T_string; t_descr=None; t_default=None};
		{t_name="config4.blah2"; t_atom=T_string; t_descr=None; t_default=None};
  	  ]
    );
    ("variants.conf", Pass,
	  [
		{t_name="config1"; t_atom=(T_variant ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
		{t_name="config2"; t_atom=(T_variant ["Bar";"Foo"]); t_descr=None; t_default=None};
		{t_name="config3"; t_atom=(T_variant_list ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
		{t_name="config4"; t_atom=(T_variant_list ["Foo";"Bar";"Alpha"]); t_descr=None; t_default=None};
	  ]
	);
    ("variants.conf", (Fail 3),
	  [
		{t_name="config1"; t_atom=(T_variant ["Bar";"Alpha"]); t_descr=None; t_default=None};
	  ]
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
    List.iter (fun (file,res,ty) ->
        printf "===> Testing: %s\n" file;
		try
		let v = new Config.config ty file in
		v#dump;
	    print_endline (Config.default_config ty);
		result file res Pass;
		with
		Config.Error (l,s) -> print_endline s; result file res (Fail l)
    ) regress_files
