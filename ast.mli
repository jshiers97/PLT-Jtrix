type operator = Add | Sub | Mul | Div | Eq | Neq | Lt | Leq | Gt | Geq | And | Or | Mod
type unaryop = Not | Neg
type ty = Int | Float | Bool | Char | Void
type bind = ty * string

type expr =
    Binop of expr * operator * expr
    | Unop of unaryop * expr
    | IntLit of int
    | FloatLit of float
    | StringLit of string
    | CharLit of char
    | BoolLit of bool
    | Variable of string
    | Assign of ty * string * expr
    | Call of string * expr list
    | Noexpr
type statement =
    | Block of statement list
    | Expr of expr
    | Return of expr
    | If of expr * statement * statement
    | For of expr * expr * expr * statement
    | While of expr * statement
    | Foreach of expr * expr * statement
type function_declaration = {
    fty: ty;
    fname: string;
    finputs: bind list; 
    body: statement list;
}
type program = function_declaration list
