(* Ocamllex scanner for MicroC *)

{ 

open Microcparse

let list_of_string f l = 
        let sep_arr = List.map String.trim (String.split_on_char ',' l) in
        List.map f sep_arr

let remove_bracket s =
        String.sub s 1 ((String.length s)-2)

let matrix_of_string f l =
        let sep_mat = List.map String.trim (String.split_on_char ';' l) in
        let prop_mat = List.map remove_bracket sep_mat in
        List.map (list_of_string f) prop_mat

}

let digit = ['0' - '9']
let digits = digit+
let flt = digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )?
let sp = (' ')*
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* 

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "string" { STRING }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '"' ['a'-'z' 'A'-'Z' '0'-'9' ' ']* '"' as str { STRINGLITERAL(str) }
| '[' (sp (digits ',' sp)* (digits)? sp as int_arr) ']'  {  INTARRLIT(list_of_string int_of_string int_arr) } 
| '[' (sp (flt ',' sp)* (flt)? sp as flt_arr) ']'  { FLTARRLIT(list_of_string float_of_string flt_arr) } 
| "int[]"  { INTARR }
| "float[]" { FLTARR }   
| '[' ((sp '[' sp (digits ',' sp)* (digits)? sp ']' ';')* (sp '[' sp (digits ',' sp)* (digits)? sp ']')? as mat)  ']' { INTMATRIXLIT(matrix_of_string int_of_string mat) }
| '[' ((sp '[' sp (flt ',' sp)* (flt)? sp ']' ';' )* (sp '[' sp (flt ',' sp)* (flt)? sp ']')? as mat) ']' { FLTMATRIXLIT(matrix_of_string float_of_string mat) }
| "Matrix<int>" { INTMATRIX }
| "Matrix<float>" { FLTMATRIX }
| "new" { NEW }
| digits as lxm { LITERAL(int_of_string lxm) }
| flt  as lxm { FLIT(lxm) }
| var  as lxm { ID(lxm) }
| var  as var '[' (digits as ind) ']' { ARRGE(var, int_of_string ind)  } 
| var as var '[' (digits as row) ']' '[' (digits as col) ']' { MATGE(var, (int_of_string row, int_of_string col)) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
