%{ open Ast %}

%token IF ELSE ELSEIF FOR WHILE MOD EQ GT GEQ LT LEQ NEQ AND OR NOT RETURN
%token INT CHAR BOOL FLOAT VOID STRING COMMA BREAK FOREACH LBRACE RBRACE LPAREN RPAREN
%token DOT LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE ASSIGN SEMI EOF
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> VARIABLE
%token <char> CHARLIT
%token <string> STRINGLIT
%token <float> FLOATLIT
%token INTARR FLOATARR
%token PRINT

%left ASSIGN SEMI
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr
%%
expr :
    expr PLUS expr { Binop($1, Add, $3) }
|   expr MINUS expr { Binop($1, Sub, $3) }
|   expr TIMES expr { Binop($1, Mul, $3) }
|   expr DIVIDE expr { Binop($1, Div, $3) }
|   expr SEMI expr { Seq($1, $3) }
|   VARIABLE ASSIGN expr { Asn($1, $3) }
|   VARIABLE { Var($1) }
|   INTLIT { Lit($1) }