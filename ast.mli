type operator = Add | Sub | Mul | Div | Eq | Neq | Less | Leq | Greater | Geq | And | Or
type unaryop = Not | Neg
type ty = Int | Float | Bool | Char | Void

type expr =
    Binop of expr * operator * expr
    | Lit of int
    | Seq of expr * expr
    | Asn of string * expr
    | Var of string
    | P of expr
    | Pr of string
    | StringLit of string


type statement =
    | PrintS of string
    | Print of expr
    | Expr of expr
