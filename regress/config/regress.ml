
open Printf

type regress_result =
    |Pass of Config.var_vals
    |Fail of string
    
let try_parse file =
    let fin = open_in file in
    let lexbuf = Lexing.from_channel fin in
    Config_location.start_parse file; 
    try Pass (Config_parser.main Config_lexer.token lexbuf)
    with Config_location.Syntax_error l ->
        Fail (sprintf "Syntax error%s near token '%s'"
        (Config_location.string_of_location l) (Lexing.lexeme lexbuf))

let regress_files = [ "strings.conf" ]
    
let _ =
    List.iter (fun file ->
        printf "Parsing: %s\n" file;
        match try_parse file with
        |Pass vars ->
            printf "pass:\n";
            print_endline (Config.string_of_var_vals vars);
        |Fail err -> printf "error: %s\n" err;
    ) regress_files