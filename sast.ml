(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SStrLit of string
  | SIntMatLit of sexpr list
  | SFltMatLit of sexpr list
  | SIntArrLit of sexpr list
  | SFltArrLit of sexpr list
  | SArrGe of string * sexpr
  | SArrSe of string * sexpr * sexpr
  | SMatGe of string * sexpr * sexpr
  | SMatSe of string * sexpr * sexpr * sexpr
  | SInitArr of string * sexpr
  | SInitMat of string * sexpr * sexpr
  | SId of string
  (*creating SCharLit*)
  | SCharLit of char
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr 
  | SCall of string * sexpr list
  | SFree of sexpr
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "{" ^ string_of_typ t ^ " : " ^ (match e with
  | SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false" 
  | SCharLit(l) -> Char.escaped l
  | SFliteral(l) -> l
  | SStrLit(l) -> l
  | SIntMatLit(m) -> "[ " ^ (String.concat "; " (List.map string_of_sexpr m)) ^ " ]"
  | SFltMatLit(m) -> "[ " ^ (String.concat "; " (List.map string_of_sexpr m)) ^ " ]"
  | SMatGe(v, r, c) -> v ^ "[" ^ (string_of_sexpr r) ^ "][" ^ (string_of_sexpr c)^ "]"
  | SMatSe(v, r, c, e) -> v ^ "[" ^ (string_of_sexpr r) ^ "][" ^ (string_of_sexpr c)^ "] = " ^ (string_of_sexpr e) 
  | SIntArrLit(l) -> "[ " ^ (String.concat ", " (List.map string_of_sexpr l)) ^ " ]"
  | SFltArrLit(l) -> "[ " ^ (String.concat ", " (List.map string_of_sexpr l)) ^ " }"
  | SArrGe(v, e) -> v ^ "[" ^ (string_of_sexpr e) ^ "]"
  | SArrSe(v, i, e) -> v ^ "[" ^ (string_of_sexpr i) ^ "] = " ^ (string_of_sexpr e) 
  | SInitArr(t, e) -> "new " ^ t ^ "[" ^ (string_of_sexpr e) ^ "]"
  | SInitMat(t, r, c) -> "new Matrix<" ^ t ^ ">[" ^ (string_of_sexpr r) ^ "][" ^ (string_of_sexpr c) ^ "]"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SFree(e) -> "free(" ^ (string_of_sexpr e) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
