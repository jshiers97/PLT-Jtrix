%{ open Ast %}

%token IF ELSE ELSEIF FOR WHILE MOD EQ GT GEQ LT LEQ NEQ AND OR NOT RETURN
%token INT CHAR BOOL FLOAT VOID STRING COMMA BREAK FOREACH LBRACE RBRACE LPAREN RPAREN
%token DOT LBRACKET RBRACKET COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN SEMI EOF
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> VARIABLE STRINGLIT
%token <char> CHARLIT
%token <float> FLOATLIT
%token <int*int> MATRIX
%token INTARR FLOATARR
%token PRINT

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%start program
%type <Ast.program> program
%%
program: methods EOF { $1 }

methods:
    | { [] }
    | methods fun_decl { $2 :: $1 }

fun_decl: ty VARIABLE LPAREN input_list RPAREN LBRACE statement_list RBRACE {
    {
        fty = $1;
        fname = $2;
        finputs = List.rev $4; 
        body = List.rev $7
    }
}

ty:
    | INT  { Int }
    | FLOAT { Float }
    | CHAR { Char }
    | BOOL { Bool }
    | VOID { Void }

vartype:
    | INT  { Int }
    | FLOAT { Float }
    | CHAR { Char }
    | BOOL { Bool }

input_list:
    | { [] }
    | inputs { $1 }

inputs: vartype VARIABLE { [($1, $2)] }
    | inputs COMMA vartype VARIABLE { ($3, $4) :: $1 }

statement_list:
    | { [] }
    | statement_list statement { $2 :: $1 }

statement:
    | expr SEMI { Expr $1 }
    | RETURN expr SEMI { Return $2 }
    | LBRACE statement_list RBRACE  { Block(List.rev $2) }
    | IF LPAREN expr RPAREN statement %prec NOELSE { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN statement ELSE statement { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN statement { For($3, $5, $7, $9) }
    | FOREACH LPAREN vartype VARIABLE COLON VARIABLE RPAREN statement { Foreach(Variable($4), Variable($6), $8) }
    | WHILE LPAREN expr RPAREN statement { While($3, $5) }

expr_opt:
    | { Noexpr }
    | expr { $1 }

expr:
    | INTLIT { IntLit($1) }
    | FLOATLIT { FloatLit($1) }
    | BOOLLIT { BoolLit($1) }
    | CHARLIT { CharLit($1) }
    | STRINGLIT { StringLit($1) }
    | VARIABLE { Variable($1) }
    | expr PLUS expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Sub, $3) }
    | expr TIMES expr { Binop($1, Mul, $3) }
    | expr DIVIDE expr { Binop($1, Div, $3) }
    | expr MOD expr { Binop($1, Mod, $3) }
    | expr EQ expr { Binop($1, Eq, $3) }
    | expr NEQ expr { Binop($1, Neq, $3) }
    | expr GT expr { Binop($1, Gt, $3) }
    | expr GEQ expr { Binop($1, Geq, $3) }
    | expr LT expr { Binop($1, Lt, $3) }
    | expr LEQ expr { Binop($1, Leq, $3) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | MINUS expr %prec NOT { Unop(Neg, $2) }
    | NOT expr { Unop(Not, $2) }
    | vartype VARIABLE ASSIGN expr { Assign($1, $2, $4) }
    | VARIABLE LPAREN args_opt RPAREN { Call($1, $3) }
    | LPAREN expr RPAREN { $2 }

args_opt:
    | { [] }
    | args_list { List.rev $1 }

args_list:
    | expr { [$1] }
    | args_list COMMA expr { $3 :: $1 }
    