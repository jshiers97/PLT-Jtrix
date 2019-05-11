(* Ocamllex scanner for MicroC *)

{ 

open Microcparse

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
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| '.'      { DOT }
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
| "int[]"  { INTARR }
| "float[]" { FLTARR }   
| "Matrix<int>" { INTMATRIX }
| "Matrix<float>" { FLTMATRIX }
| "new" { NEW }
| digits as lxm { LITERAL(int_of_string lxm) }
| flt  as lxm { FLIT(lxm) }
| var  as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
