
open Printf
open Config_t

(* We expect the test to either pass or fail at a particular line *)
type result =
	|Pass
	|Fail of int
	
let regress_files = [
	("strings.conf", Pass,
	  [
		"config1", T_string;
		"config2", T_string;
		"config3", T_string;
		"config4.blah", T_string;
		"config4.blah2", T_string;
  	  ]
    );
    ("variants.conf", Pass,
	  [
		"config1", (T_variant ["Foo";"Bar";"Alpha"]);
		"config2", (T_variant ["Bar";"Foo"]);
		"config3", (T_variant_list ["Foo";"Bar";"Alpha"]);
		"config4", (T_variant_list ["Foo";"Bar";"Alpha"]);
	  ]
	);
    ("variants.conf", (Fail 3),
	  [
		"config1", (T_variant ["Bar";"Alpha"]);
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
		result file res Pass;
		with
		Config.Error (l,s) -> print_endline s; result file res (Fail l)
    ) regress_files
