(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "jtrix" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  in
  let string_t   = L.pointer_type (i8_t)
  and int_arr_t  = L.pointer_type (i32_t)
  and float_arr_t = L.pointer_type (float_t)
  in
  let int_mat_t = L.pointer_type int_arr_t
  and float_mat_t = L.pointer_type float_arr_t
  in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    (* added chars to this *)
    | A.Char  -> i8_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.IntArr -> int_arr_t
    | A.FltArr -> float_arr_t
    | A.IntMat -> int_mat_t
    | A.FltMat -> float_mat_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, _) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
  let idx_check_t : L.lltype =
          L.var_arg_function_type void_t [| i32_t ; i32_t |] in
  let idx_check_func : L.llvalue =
          L.declare_function "idx_check" idx_check_t the_module in

  let switch_rows_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; i32_t; i32_t |] in
  let switch_rows_i_func : L.llvalue =
          L.declare_function "switch_rows_i" switch_rows_i_t the_module in

  let switch_rows_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; i32_t; i32_t |] in
  let switch_rows_f_func : L.llvalue =
          L.declare_function "switch_rows_f" switch_rows_f_t the_module in

  let printarr_t : L.lltype =
          L.var_arg_function_type i32_t [| L.pointer_type i32_t |] in
  let printarr_func : L.llvalue =
      L.declare_function "printarr" printarr_t the_module in

  let printfltarr_t : L.lltype =
          L.var_arg_function_type float_t [| i32_t ; L.pointer_type float_t |] in
  let printfltarr_func : L.llvalue =
          L.declare_function "printfltarr" printfltarr_t the_module in

  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	Array.of_list (List.map (fun (t, _, _) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and new_line = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in
      (*maps type, ID, expr into type, ID*)
      let tempformals = List.map (fun(t, v, _) -> (t, v)) fdecl.sformals in
      let templocals = List.map(fun(t,v,_) -> (t,v)) fdecl.slocals
      in
      let formals = List.fold_left2 add_formal StringMap.empty tempformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals templocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Creates float array *)
    let create_flt_arr s l =
        let t = Array.of_list [(L.const_int i32_t 0)] in
        let ptr = L.build_gep s t "" builder in
        ignore(L.build_store (L.const_float float_t (float_of_int (List.length l))) ptr builder);
        for i = 1 to ((List.length l)) do
            let t = Array.of_list  [(L.const_int i32_t i)] in
            let ptr = L.build_gep s t "" builder in
            ignore(L.build_store (L.const_float float_t (List.nth l (i-1))) ptr builder)
         done;
         s
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SCharLit l -> L.const_int i8_t (int_of_char l)
      | SStrLit l   -> let str_wo_qt = List.nth (String.split_on_char '"' l) 1 in
                       L.build_global_stringptr str_wo_qt str_wo_qt  builder
      | SIntMatLit (m) -> let s = L.build_array_alloca int_arr_t (L.const_int i32_t ((List.length m)+1)) "" builder in
                         for i = 1 to ((List.length m)) do
                            let t = [|L.const_int i32_t i|] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth m (i - 1))) ptr builder)
                         done;
                         let dim = L.build_array_alloca i32_t (L.const_int i32_t 3) "" builder in
                         for i = 0 to 3 do
                            let t = [|L.const_int i32_t i|] in
                            let ptr = L.build_gep dim t "" builder in
                            if i = 0 then
                            ignore(L.build_store (L.const_int i32_t 2) ptr builder)
                            else if i = 1 then ignore(L.build_store (L.const_int i32_t (List.length m)) ptr builder)
                            else
                                 let eval (_, e) = match e with
                                 | SIntArrLit l -> List.length l
                                 | _ -> raise (Failure "impossible")
                                 in
                                 ignore(L.build_store (L.const_int i32_t (eval (List.hd m))) ptr builder)

                        done;
                         let dim_ptr = L.build_gep s [|L.const_int i32_t 0|] "" builder in
                         ignore(L.build_store dim s builder);
                         s
      | SFltMatLit (m) ->  let s = L.build_array_alloca float_arr_t (L.const_int i32_t ((List.length m))) "" builder in
                         for i = 1 to ((List.length m)) do
                            let t = Array.of_list [L.const_int i32_t (i-1)] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth m (i - 1))) ptr builder)
                         done;
                         let dim = L.build_array_alloca float_t (L.const_int i32_t 3) "" builder in
                         for i = 0 to 3 do
                            let t = [|L.const_int i32_t i|] in
                            let ptr = L.build_gep dim t "" builder in
                            if i = 0 then
                            ignore(L.build_store (L.const_float float_t 2.0) ptr builder)
                            else if i = 1 then ignore(L.build_store (L.const_float float_t (float_of_int (List.length m))) ptr builder)
                            else
                                 let eval (_, e) = match e with
                                 | SFltArrLit l -> float_of_int (List.length l)
                                 | _ -> raise (Failure "impossible")
                                 in
                                 ignore(L.build_store (L.const_float float_t (eval (List.hd m))) ptr builder)

                        done;
                         let dim_ptr = L.build_gep s [|L.const_int i32_t 0|] "" builder in
                         ignore(L.build_store dim s builder);
                         s
      | SMatGe (v, r, c) -> let row_num = Array.of_list [L.build_add (expr builder r) (L.const_int i32_t 1) "row" builder] in
                            let col_num = Array.of_list [L.build_add (expr builder c) (L.const_int i32_t 1) "idx" builder] in
                            let mat = L.build_load (lookup v) "" builder in
                            let row_ptr = L.build_gep mat row_num "" builder in
                            let row = L.build_load row_ptr "" builder in
                            let col_ptr = L.build_gep row col_num "" builder in
                            L.build_load col_ptr "" builder
      | SMatSe (v, r, c, (ty, e)) -> let row_num = Array.of_list [(expr builder r)] in
                            let col_num = Array.of_list [L.build_add (expr builder c) (L.const_int i32_t 1) "idx" builder] in
                            let mat = L.build_load (lookup v) "" builder in
                            let row_ptr = L.build_gep mat row_num "" builder in
                            let row = L.build_load row_ptr "" builder in
                            let col_ptr = L.build_gep row col_num "" builder in
                            let typ_e = expr builder (ty, e) in
                            L.build_store (typ_e) col_ptr builder
     | SIntArrLit (a) -> let s = L.build_array_alloca i32_t (L.const_int i32_t ((List.length a)+1)) "" builder in
                         let t = Array.of_list [(L.const_int i32_t 0)] in
                         let ptr = L.build_gep s t "" builder in
                         ignore(L.build_store (L.const_int i32_t (List.length a)) ptr builder);
                         for i = 1 to ((List.length a)) do
                            let t = Array.of_list [L.const_int i32_t i] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth a (i - 1))) ptr builder)
                         done;
                         s
      | SFltArrLit (a) -> let s = L.build_array_alloca float_t (L.const_int i32_t ((List.length a)+1)) "" builder in
                         let t = Array.of_list [(L.const_int i32_t 0)] in
                         let ptr = L.build_gep s t "" builder in
                         ignore(L.build_store (L.const_float float_t (float_of_int (List.length a))) ptr builder);
                         for i = 1 to ((List.length a)) do
                            let t = Array.of_list [L.const_int i32_t i] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth a (i - 1))) ptr builder)
                         done;
                         s
      | SArrGe(v, e) -> let arr = L.build_load (lookup v) v builder in
                        let idx = L.build_add (expr builder e) (L.const_int i32_t 1) "" builder in
                        let size_ptr = L.build_gep arr (Array.of_list[L.const_int i32_t 0]) "" builder in
                        let size = L.build_load size_ptr "" builder in
                        ignore(L.build_call idx_check_func [| idx; size |] "" builder);
                        let t = [|idx|] in
                        let ptr = L.build_gep arr t "" builder in
                        L.build_load ptr "" builder
      | SArrSe(v, i, e) -> let arr = L.build_load (lookup v) v builder in
                           let idx = L.build_add (expr builder i) (L.const_int i32_t 1) "idx" builder in
                           let size_ptr = L.build_gep arr (Array.of_list[L.const_int i32_t 0]) "" builder in
                        let size = L.build_load size_ptr "" builder in
                        ignore(L.build_call idx_check_func [| idx; size |] "" builder);
                           let t = Array.of_list [idx] in
                           let ptr = L.build_gep arr t "" builder in
                           L.build_store (expr builder e) ptr builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg when t = A.Float -> L.build_fneg
	  | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) ->
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("println", [e]) ->
                      L.build_call printf_func [| new_line ; (expr builder e) |] "printf" builder
      | SCall ("printarr", [e]) ->
              L.build_call printarr_func [| (expr builder e) |] "printarr" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SStdLib (v, f, args) ->
         match f with
         | "row" -> let row_num = [|L.build_add (expr builder (List.hd args)) (L.const_int i32_t 1) "" builder|] in
                    let mat = expr builder v in
                    let row_ptr = L.build_gep mat row_num "" builder in
                    L.build_load row_ptr "" builder
         | "dim" -> let dim = [| L.const_int i32_t 0 |] in
                    let mat = expr builder v in
                    let dim_ptr = L.build_gep mat dim "" builder in
                    L.build_load dim_ptr "" builder
         | "switchRows" ->
                    (match (fst v) with
                    | A.IntMat -> L.build_call switch_rows_i_func [| expr builder v; expr builder (List.hd args); expr builder (List.nth args 1) |] "switch_rows_i" builder
                    | A.FltMat -> L.build_call switch_rows_f_func [| expr builder v; expr builder (List.hd args); expr builder (List.nth args 1) |] "switch_rows_f" builder
                    | _ -> raise (Failure "Expression is not a matrix")
                    )
         | _ -> raise (Failure "not implemented")


    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
