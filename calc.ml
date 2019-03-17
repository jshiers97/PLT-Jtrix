open Ast

module StringMap = Map.Make(String)

let rec eval symbol_table expr =
    match expr with
    Lit(x) -> (symbol_table, x)
    | Var(v) -> (symbol_table, StringMap.find v symbol_table)
    | Asn(v, e1) ->
        let second_part = eval symbol_table e1 in
        let new_symbol_table = (StringMap.add v (snd second_part) (fst second_part)) in
            (new_symbol_table, StringMap.find v new_symbol_table)
    | Seq(e1, e2) -> eval (fst (eval symbol_table e1)) e2
    | Binop(e1, op, e2) ->
        let v1 = eval symbol_table e1 and v2 = eval symbol_table e2 in
            match op with
                Add -> (symbol_table, (snd v1) + (snd v2))
            |   Sub -> (symbol_table, (snd v1) - (snd v2))
            |   Mul -> (symbol_table, (snd v1) * (snd v2))
            |   Div -> (symbol_table, (snd v1) / (snd v2))

let _ =
    let lexbuf = Lexing.from_channel stdin in  
        let expr = Parser.expr Scanner.token lexbuf in
            let result = snd (eval StringMap.empty expr) in
                print_endline (string_of_int result)