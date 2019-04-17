(* Ocamllex scanner for MicroC *)

{ open Microcparse  }

let digit = ['0' - '9']
let digits = digit+
let flt = digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )?
let sp = (' ')*

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
| '[' (sp (digits ',' sp)* (digits)? sp as int_arr) ']'  {  INTARRLIT(
        let  sep_arr = List.map String.trim (String.split_on_char ','  int_arr) in
        List.map int_of_string sep_arr
        ) } 
| '[' (sp (flt ',' sp)* (flt)? sp as flt_arr) ']'  {  FLTARRLIT(
        let  sep_arr = List.map String.trim (String.split_on_char ','  flt_arr) in
        List.map float_of_string sep_arr
        ) }
| "int[]" { INTARR }
| "float[]" { FLTARR }
| digits as lxm { LITERAL(int_of_string lxm) }
| flt  as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
