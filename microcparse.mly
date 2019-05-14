/* Ocamlyacc parser for MicroC */

%{

open Ast

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR INTARR FLTARR INTMATRIX FLTMATRIX NEW
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID STRING LBRACK RBRACK DOT
%token FREE
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT STRINGLITERAL 
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%nonassoc DOT
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }
  | STRING { String }
  | INTARR { IntArr }
  | FLTARR { FltArr }
  | INTMATRIX { IntMat }
  | FLTMATRIX { FltMat }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | STRINGLITERAL    { StrLit($1)             }
  | LBRACK mat RBRACK { $2    }
  | ID LBRACK expr RBRACK {  ArrGe($1, $3)    } 
  | ID LBRACK expr RBRACK ASSIGN expr { ArrSe($1, $3, $6) }
  | ID LBRACK expr RBRACK LBRACK expr RBRACK { MatGe($1, $3, $6) }
  | ID LBRACK expr RBRACK LBRACK expr RBRACK ASSIGN expr { MatSe($1, $3, $6, $9) }
  | expr DOT ID LPAREN args_opt RPAREN  { Call($3, ($1 :: $5)) }
  | NEW INT LBRACK expr RBRACK { InitArr("int", $4) }
  | NEW FLOAT LBRACK expr RBRACK  { InitArr("float", $4) }
  | NEW INTMATRIX LBRACK expr RBRACK LBRACK expr RBRACK { InitMat("int", $4, $7) }
  | NEW FLTMATRIX LBRACK expr RBRACK LBRACK expr RBRACK { InitMat("float", $4, $7) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr PLUS PLUS   { Binop($1, Add, Literal(1)) }
  | expr MINUS MINUS { Binop($1, Sub, Literal(1)) }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | FREE LPAREN expr RPAREN { Free($3)        }
  | LPAREN expr RPAREN { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

arr_opt:
      { [] }
  | arr_list  { List.rev $1 }

arr_list:
    expr { [$1] }
  | arr_list COMMA expr { $3 :: $1 }

mat:
  | arr_opt { ArrLit($1) }
  | mat_list { let mat = List.rev $1 in
                        let arrlit a = ArrLit(a) in
                        MatLit(List.map arrlit mat)  }
 
mat_list:
  | LBRACK arr_opt RBRACK { [$2] }
  | mat_list SEMI LBRACK arr_list RBRACK { (List.rev $4) ::  $1 }
