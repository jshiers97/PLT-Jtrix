(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty, r) = StringMap.add name {
      typ = r;
      fname = name;
      formals = (let rec make_bind =
             function
             | [] -> []
             | hd :: tl -> (hd, "x") :: (make_bind tl)
             in make_bind ty);
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", [Int], Void);
                                                 ("printb", [Bool], Void);
                                                 ("printf", [Float], Void);
                                                 ("printc", [Char], Void);
                                                 ("println", [String], Void);
                                                 ("row_i", [IntMat; Int], IntArr);
                                                 ("row_f", [FltMat; Int], FltArr);
                                                 ("dim_i", [IntMat], IntArr);
                                                 ("dim_f", [FltMat], FltArr);
                                                 ("col_i", [IntMat], IntArr);
                                                 ("col_f", [FltMat], FltArr);
                                                 ("spliceRow_i", [IntMat; Int], IntMat);
                                                 ("spliceRow_f", [FltMat; Int], FltMat);
                                                 ("spliceColumn_i", [IntMat; Int], IntMat);
                                                 ("spliceColumn_f", [FltMat; Int], FltMat);
                                                 ("transpose_i", [IntMat], IntMat);
                                                 ("transpose_f", [FltMat], FltMat);
                                                 ("switchRows_i", [IntMat; Int; Int], IntMat);
                                                 ("switchRows_f", [FltMat; Int; Int], FltMat);
                                                 ("size_i", [IntArr], Int);
                                                 ("size_f", [FltArr], Int);
                                                 ("int_to_f", [Int], Float);
                                                 ("f_to_int", [Float], Int); 
                                                 ]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in
    match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      (*Semantically checked Char Literal added*)
      | CharLit l -> (Char, SCharLit l)
      | StrLit l   -> (String, SStrLit l)
      | MatLit (m) -> let first_typ = fst (expr (List.hd m)) in
                      let check_consistent a =
                         if((fst(expr a)) != first_typ) then
                            raise (Failure "Inconsistent types in matrix") in
                      ignore(List.iter check_consistent m);
                      let eval a = match a with
                      | ArrLit (a) -> List.length a
                      | _ -> raise (Failure "Expected an array") in
                      let check_col_consist e =
                         if((eval e) != (eval (List.hd m))) then
                            raise (Failure "Each row must have the same number of columns") in
                      ignore(List.iter check_col_consist m);
                      let sem_ele = List.map expr m in
                      (match (first_typ) with
                      | IntArr -> (IntMat, SIntMatLit(sem_ele))
                      | FltArr -> (FltMat, SFltMatLit(sem_ele))
                      | _ -> raise (Failure "Invalid type for matrix"))
      | ArrLit (l) -> let first_typ = fst (expr (List.hd l)) in
                      let check_consistent a =
                         if((fst(expr a)) != first_typ) then
                            raise (Failure "Inconsistent types in array") in
                      ignore(List.iter check_consistent l);
                      let sem_ele = List.map expr l in
                      (match (first_typ) with
                      | Int -> (IntArr, SIntArrLit(sem_ele))
                      | Float -> (FltArr, SFltArrLit(sem_ele))
                      | _ -> raise (Failure "Invalid type for array"))
      | ArrGe (v, e) ->
          let ele_typ = match (type_of_identifier v) with
          | IntArr -> Int
          | FltArr -> Float
          | _ -> raise (Failure "Variable is not an array") in
          let ind_typ = match (expr e) with
          | (Int, _) -> (expr e)
          | _ -> raise (Failure "Index must be an integer") in
          (ele_typ, SArrGe(v, ind_typ))
      | ArrSe (v, i, e) ->
          let ele_type_arr = match (type_of_identifier v) with
          | IntArr -> Int
          | FltArr -> Float
          | _ -> raise (Failure "Variable is not an array") in
          let idx_type = match (expr i) with
          | (Int, _) -> true
          | _ -> false in
          if(idx_type) then (
                if (ele_type_arr = (fst (expr e))) then
                        (ele_type_arr, SArrSe(v, (expr i), (expr e)))
                else raise (Failure ("expected type " ^ (string_of_typ ele_type_arr)
                 ^ " but received an expression of type " ^ string_of_typ (fst (expr e))))
                )
          else raise (Failure "Index must be an integer")
      | MatGe (v, r, c) ->
         let ele_typ = match (type_of_identifier v) with
         | IntMat -> Int
         | FltMat -> Float
         | _ -> raise (Failure "Variable is not an matrix") in
         let idx_type = match (expr r), (expr c) with
         | (Int, _), (Int, _) -> true
         | _ -> false in
         if(idx_type) then (
         (ele_typ, SMatGe(v, (expr r), (expr c)))
         )
         else raise (Failure ("Index must be an integer"))
      | MatSe (v, r, c, e) ->
         let ele_typ_mat = match (type_of_identifier v) with
         | IntMat -> Int
         | FltMat -> Float
         | _ -> raise (Failure "Variable is not a matrix") in
         let idx_type = match (expr r), (expr c) with
         | (Int, _), (Int, _) -> true
         | _ -> false in
         if(idx_type) then (
         if (ele_typ_mat = (fst (expr e))) then
                 (ele_typ_mat, SMatSe(v, (expr r), (expr c), (expr e)))
         else raise (Failure "Expression is of wrong type")
         )
         else raise (Failure "Index must be an integer")
      | InitArr(typ, e) ->
                let check_e = match (expr e) with
                | (Int, _) -> expr e
                | _ -> raise (Failure "Value must be an integer") in
                let check_arr_type = match typ with
                | "int" -> IntArr
                | "float" -> FltArr
                | _ -> raise (Failure "Invalid array type") in
                (check_arr_type, SInitArr(typ, check_e))
      | InitMat(typ, r, c) ->
                let check_e e = match (expr e) with
                 | (Int, _) -> expr e
                | _ -> raise (Failure "Value must be an integer") in
                let check_arr_type = match typ with
                | "int" -> IntMat
                | "float" -> FltMat
                | _ -> raise (Failure "Invalid array type") in
                (check_arr_type, SInitMat(typ, check_e r, check_e c))
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Free(e) ->
          let e' = expr e in
          (match (fst e') with
          | IntArr -> (Void, SFree(e'))
          | IntMat -> (Void, SFree(e'))
          | FltArr -> (Void, SFree(e'))
          | FltMat -> (Void, SFree(e'))
          | _ -> raise (Failure "Cannot free expression"))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
