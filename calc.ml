open Ast

module StringMap = Map.Make(String)

let rec eval symbol_table expr =
    (1, 2)

let _ =
    let lexbuf = Lexing.from_channel stdin in  
        let expr = Parser.program Scanner.token lexbuf in
            let result = snd (eval StringMap.empty expr) in
                print_endline (string_of_int result)