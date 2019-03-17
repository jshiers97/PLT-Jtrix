{ open Parser }

rule token =
    parse [' ' '\t' '\r' '\n'] { token lexbuf}
        | '+' { PLUS }
        | '-' { MINUS }
        | '*' { TIMES }
        | '/' { DIVIDE }
        | '%' { MOD }
        | "==" { EQ }
        | '>' { GT }
        | ">=" { GEQ }
        | '<' { LT } 
        | "<=" { LEQ }
        | "!=" { NEQ }
        | "&&" { AND }
        | "||" { OR }
        | '!' { NOT }
        | '=' { ASSIGN }
        | ['0' - '9']+ as lit { INTLIT( int_of_string lit ) }
        | ['a' - 'z']+ as id { VARIABLE( id ) }
        | ';' { SEMI }
        | "if" { IF }
        | "else" { ELSE }
        | "for" { FOR }
        | "while" { WHILE }
        | "return" { RETURN }
        | "int" { INT }
        | "float" { FLOAT }
        | "char" { CHAR }
        | "string" { STRING }
        | "void" { VOID }
        | ',' { COMMA }
        | "break" { BREAK }
        | "boolean" { BOOL }
        | "foreach" { FOREACH }
        | "true" | "false" as bool { BOOLLIT(if bool = "true" then true else false)}
        | '\'' (['A' - 'Z' 'a' - 'z' '.' '!' '?' '\\' '-' '#' '$' '%' '(' ')' '*' '+' ',' '/' ';' ':' '<' '>' '?' '@' '[' ']' '^' '_' '"' '~' '`'] as char) '\'' { CHARLIT(char) } 
        | '"' (['A' - 'Z' 'a' - 'z' '.' '!' '?' '\\' '-' '#' '$' '%' '(' ')' '*' '+' ',' '/' ';' ':' '<' '>' '?' '@' '[' ']' '^' '_' '"' '~' '`' '\n' '\"' '\'']* as string) '"' { STRINGLIT(string) }
        | ['0' - '9']+ '.' ['0' - '9']* as float { FLOATLIT(float_of_string float) }
        | '{' { LBRACE }
        | '}' { RBRACE }
        | '(' { LPAREN }
        | ')' { RPAREN }
        | '[' { LBRACKET }
        | ']' { RBRACKET }
        | '.' { DOT }
        | "int[]" { INTARR }
        | "float[]" { FLOATARR }
        | "print" { PRINT }
        | "//" { inline_comment lexbuf }
        | "/*" { multiline_comment lexbuf }
        | eof { EOF }
    and inline_comment =
        parse '\n' { token lexbuf }
        | _ { inline_comment lexbuf }
    and multiline_comment =
        parse "*/" { token lexbuf }
        | _ { multiline_comment lexbuf }