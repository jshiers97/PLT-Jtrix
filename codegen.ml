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

  (* Return the LLVM type for a Jtrix type *)
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
    let global_var m (t, n) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
  
  
  let mult_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; int_mat_t |] in
  let mult_i_func : L.llvalue =
          L.declare_function "mult_i" mult_i_t the_module in
  
  let mult_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; float_mat_t |] in
  let mult_f_func : L.llvalue =
          L.declare_function "mult_f" mult_f_t the_module in

  let op_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; int_mat_t; i32_t |] in
  let op_i_func : L.llvalue =
          L.declare_function "op_i" op_i_t the_module in
  
  let op_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; float_mat_t; i32_t |] in
  let op_f_func : L.llvalue =
          L.declare_function "op_f" op_f_t the_module in
  
  let idx_check_t : L.lltype =
          L.var_arg_function_type void_t [| i32_t ; i32_t |] in
  let idx_check_func : L.llvalue =
          L.declare_function "idx_check" idx_check_t the_module in

  let init_mat_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| i32_t; i32_t |] in
  let init_mat_i_func : L.llvalue =
          L.declare_function "init_mat_i" init_mat_i_t the_module in

  let init_mat_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| i32_t; i32_t |] in
  let init_mat_f_func : L.llvalue =
          L.declare_function "init_mat_f" init_mat_f_t the_module in

  let switch_rows_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; i32_t; i32_t |] in
  let switch_rows_i_func : L.llvalue =
          L.declare_function "switch_rows_i" switch_rows_i_t the_module in

  let switch_rows_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; i32_t; i32_t |] in
  let switch_rows_f_func : L.llvalue =
          L.declare_function "switch_rows_f" switch_rows_f_t the_module in

  let transpose_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t |] in
  let transpose_i_func : L.llvalue =
          L.declare_function "transpose_i" transpose_i_t the_module in

  let transpose_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t |] in
  let transpose_f_func : L.llvalue =
          L.declare_function "transpose_f" transpose_f_t the_module in

  let splice_row_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; i32_t |] in
  let splice_row_i_func : L.llvalue =
          L.declare_function "splice_row_i" splice_row_i_t the_module in

  let splice_row_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; i32_t |] in
  let splice_row_f_func : L.llvalue =
          L.declare_function "splice_row_f" splice_row_f_t the_module in

  let splice_col_i_t : L.lltype =
          L.var_arg_function_type int_mat_t [| int_mat_t; i32_t |] in
  let splice_col_i_func : L.llvalue =
          L.declare_function "splice_col_i" splice_col_i_t the_module in

  let splice_col_f_t : L.lltype =
          L.var_arg_function_type float_mat_t [| float_mat_t; i32_t |] in
  let splice_col_f_func : L.llvalue =
          L.declare_function "splice_col_f" splice_col_f_t the_module in

  let col_i_t : L.lltype =
          L.var_arg_function_type int_arr_t [| int_mat_t; i32_t |] in
  let col_i_func : L.llvalue =
          L.declare_function "col_i" col_i_t the_module in

  let col_f_t : L.lltype =
          L.var_arg_function_type float_arr_t [| float_mat_t; i32_t |] in
  let col_f_func : L.llvalue =
          L.declare_function "col_f" col_f_t the_module in
(*
  let f_to_int_t : L.lltype =
          L.function_type float_t [| i32_t |] in
  let f_to_int_func : L.llvalue =
          L.declare_function "f_to_int" f_to_in_t the_module in

  let int_to_f_t : L.lltype =
          L.function_type float_t [| i32_t |] in
  let int_to_f_func : L.llvalue =
          L.declare_function "int_to_f" int_to_f_t the_module in
*)
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%.3f\n" "fmt" builder
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

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Returns the dimension array stored in matrix *)
    let get_dim mat =
              let dim = [| L.const_int i32_t 0 |] in
              let dim_ptr = L.build_gep mat dim "" builder in
              L.build_load dim_ptr "" builder
    in

    (* Returns the row stored in the matrix *)
    let get_row mat row_num =
              let row_ptr = L.build_gep mat row_num "" builder in
              L.build_load row_ptr "" builder
    in

    (* Returns the size of array *)
    let get_size arr =
         let s = L.build_gep arr [| L.const_int i32_t 0 |] "" builder in
         L.build_load s "" builder
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SCharLit l -> L.const_int i8_t (int_of_char l)
      | SStrLit l   -> let str_wo_qt = List.nth (String.split_on_char '"' l) 1 in
                       L.build_global_stringptr str_wo_qt str_wo_qt  builder
      | SIntMatLit (m) -> let s = L.build_array_malloc int_arr_t (L.const_int i32_t ((List.length m)+1)) "" builder in
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
                         ignore(L.build_store dim dim_ptr builder);
                         s
      | SFltMatLit (m) ->  let s = L.build_array_malloc float_arr_t (L.const_int i32_t ((List.length m)+1)) "" builder in
                         for i = 1 to ((List.length m)) do
                            let t = [|L.const_int i32_t i|] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth m (i - 1))) ptr builder)
                         done;
                         let dim = L.build_array_alloca float_t (L.const_int i32_t 3) "" builder in
                         for i = 0 to 3 do
                            let t = [|L.const_int i32_t i|] in
                            let ptr = L.build_gep dim t "" builder in
                            if i = 0 then
                            ignore(L.build_store (L.const_float float_t 2.0) ptr builder)
                            else if i = 1 then ignore(L.build_store (L.const_float float_t (float_of_int(List.length m))) ptr builder)
                            else
                                 let eval (_, e) = match e with
                                 | SFltArrLit l -> float_of_int (List.length l)
                                 | _ -> raise (Failure "impossible")
                                 in
                                 ignore(L.build_store (L.const_float float_t (eval (List.hd m))) ptr builder)

                        done;
                         let dim_ptr = L.build_gep s [|L.const_int i32_t 0|] "" builder in
                         ignore(L.build_store dim dim_ptr builder);
                         s
      | SMatGe (v, r, c) -> let r_n = L.build_add (expr builder r) (L.const_int i32_t 1) "row" builder in
                            let c_n = L.build_add (expr builder c) (L.const_int i32_t 1) "idx" builder in
                            let row_num = [| r_n |] in
                            let col_num = [| c_n |] in
                            let mat = L.build_load (lookup v) "" builder in
                            let dim = L.build_gep mat [| L.const_int i32_t 0 |] "" builder in
                            let dim_arr = L.build_load dim "" builder in
                            let r_size = L.build_gep dim_arr [| L.const_int i32_t 1|] "" builder in
                            let c_size = L.build_gep dim_arr [| L.const_int i32_t 2|] "" builder  in
                            if ((L.type_of (lookup v)) = int_mat_t) then (
                            ignore(L.build_call idx_check_func [| r_n; L.build_load r_size "" builder |] "" builder);
                            ignore(L.build_call idx_check_func [| c_n; L.build_load c_size "" builder |] "" builder);
                            )
                            else (
                            ignore(L.build_call idx_check_func [| r_n; L.build_fptosi (L.build_load r_size "" builder) i32_t "" builder |] "" builder);
                            ignore(L.build_call idx_check_func [| c_n; L.build_fptosi (L.build_load c_size "" builder) i32_t "" builder |] "" builder);
                            );
                            let row_ptr = L.build_gep mat row_num "" builder in
                            let row = L.build_load row_ptr "" builder in
                            let col_ptr = L.build_gep row col_num "" builder in
                            L.build_load col_ptr "" builder
      | SMatSe (v, r, c, (ty, e)) -> let r_n = L.build_add (expr builder r) (L.const_int i32_t 1) "row" builder in
                            let c_n = L.build_add (expr builder c) (L.const_int i32_t 1) "idx" builder in
                            let row_num = [| r_n |] in
                            let col_num = [| c_n |] in
                            let mat = L.build_load (lookup v) "" builder in
                            let dim = L.build_gep mat [| L.const_int i32_t 0 |] "" builder in
                            let dim_arr = L.build_load dim "" builder in
                            let r_size = L.build_gep dim_arr [| L.const_int i32_t 1|] "" builder in
                            let c_size = L.build_gep dim_arr [| L.const_int i32_t 2|] "" builder  in
                            if ((L.type_of (lookup v)) = int_mat_t) then (
                            ignore(L.build_call idx_check_func [| r_n; L.build_load r_size "" builder |] "" builder);
                            ignore(L.build_call idx_check_func [| c_n; L.build_load c_size "" builder |] "" builder);
                            )
                            else (
                            ignore(L.build_call idx_check_func [| r_n; L.build_fptosi (L.build_load r_size "" builder) i32_t "" builder |] "" builder);
                            ignore(L.build_call idx_check_func [| c_n; L.build_fptosi (L.build_load c_size "" builder) i32_t "" builder |] "" builder);
                            );
                            let row_ptr = L.build_gep mat row_num "" builder in
                            let row = L.build_load row_ptr "" builder in
                            let col_ptr = L.build_gep row col_num "" builder in
                            let typ_e = expr builder (ty, e) in
                            L.build_store (typ_e) col_ptr builder
     | SIntArrLit (a) -> let s = L.build_array_malloc i32_t (L.const_int i32_t ((List.length a)+1)) "" builder in
                         let t = Array.of_list [(L.const_int i32_t 0)] in
                         let ptr = L.build_gep s t "" builder in
                         ignore(L.build_store (L.const_int i32_t (List.length a)) ptr builder);
                         for i = 1 to ((List.length a)) do
                            let t = Array.of_list [L.const_int i32_t i] in
                            let ptr = L.build_gep s t "" builder in
                            ignore(L.build_store (expr builder (List.nth a (i - 1))) ptr builder)
                         done;
                         s
      | SFltArrLit (a) -> let s = L.build_array_malloc float_t (L.const_int i32_t ((List.length a)+1)) "" builder in
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
                        let size_ptr = L.build_gep arr [|L.const_int i32_t 0|] "" builder in
                        let size = L.build_load size_ptr "" builder in
                        if(L.type_of (lookup v) = int_arr_t) then (
                               ignore(L.build_call idx_check_func [| idx; size |] "" builder);
                        )
                        else (
                                ignore(L.build_call idx_check_func [| idx; L.build_fptosi size i32_t "" builder |] "" builder);
                        );
                        let t = [|idx|] in
                        let ptr = L.build_gep arr t "" builder in
                        L.build_load ptr "" builder
      | SArrSe(v, i, e) -> let arr = L.build_load (lookup v) v builder in
                           let idx = L.build_add (expr builder i) (L.const_int i32_t 1) "idx" builder in
                           let size_ptr = L.build_gep arr (Array.of_list[L.const_int i32_t 0]) "" builder in
                           let size = L.build_load size_ptr "" builder in
                           if(L.type_of (lookup v) = int_arr_t) then (
                              ignore(L.build_call idx_check_func [| idx; size |] "" builder);
                           )
                           else (
                              ignore(L.build_call idx_check_func [| idx; L.build_fptosi size i32_t "" builder |] "" builder);
                           );
                           let t = Array.of_list [idx] in
                           let ptr = L.build_gep arr t "" builder in
                           L.build_store (expr builder e) ptr builder
      | SInitArr(t, e) -> let e' = expr builder e in
                          (match t with
                          | "int" -> let s = L.build_array_malloc i32_t e' "" builder in
                                     let t = L.build_gep s [|L.const_int i32_t 0|] "" builder in
                                     ignore(L.build_call idx_check_func [| e'; e' |] "" builder);
                                     ignore(L.build_store e' t  builder);
                                     s
                          | "float" -> let cast = L.build_sitofp e' float_t "" builder in
                                       let s = L.build_array_malloc float_t e' "" builder in
                                       let t = L.build_gep s [|L.const_int i32_t 0|] "" builder in
                                       ignore(L.build_call idx_check_func [| e'; e' |] "" builder);
                                       ignore(L.build_store cast t builder);
                                       s
                          | _ -> raise (Failure "Invalid array type")
                          )
      | SInitMat(t, r, c) -> let r' = expr builder r and c' = expr builder c in
                             (match t with
                             | "int" -> L.build_call init_mat_i_func [| r'; c'|] "init_mat_i" builder
                             | "float" -> L.build_call init_mat_f_func [| r'; c' |] "init_mat_f" builder
                             | _ -> raise (Failure "Invalid matrix type")
                             )
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
      | SBinop ((A.IntMat, _) as e1, op, e2) ->
          let e1' = expr builder e1 and e2' = expr builder e2 in
          (match op with
          | A.Add -> L.build_call op_i_func [| e1' ; e2'; L.const_int i32_t 1 |] "op_i" builder
          | A.Sub -> L.build_call op_i_func [| e1' ; e2'; L.const_int i32_t 0 |] "op_i" builder
          | A.Mult -> L.build_call mult_i_func [| e1'; e2'|] "mult_i" builder
          | _ -> raise (Failure "interal error")
          )
      | SBinop ((A.FltMat, _) as e1, op, e2) ->
          let e1' = expr builder e1 and e2' = expr builder e2 in
          (match op with
          | A.Add -> L.build_call op_f_func [| e1' ; e2'; L.const_int i32_t 1 |] "op_i" builder
          | A.Sub -> L.build_call op_f_func [| e1' ; e2'; L.const_int i32_t 0 |] "op_i" builder
          | A.Mult -> L.build_call mult_f_func [| e1'; e2' |] "mult_f" builder
          | _ -> raise (Failure "interal error")
          )
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
	            L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | SCall ("printc", [e]) ->
	            L.build_call printf_func [| char_format_str ; (expr builder e) |] "printf" builder
      | SCall ("printf", [e]) ->
	            L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
      | SCall ("println", [e]) ->
                      L.build_call printf_func [| new_line ; (expr builder e) |] "printf" builder
      | SCall ("transpose_i", [e]) ->
              L.build_call transpose_i_func [| expr builder e |] "tranpose_i" builder
      | SCall ("transpose_f", [e]) ->
              L.build_call transpose_f_func [| expr builder e |] "transpose_f" builder
      | SCall ("row_i", [m;r]) ->
              let row_num = [|L.build_add (expr builder r) (L.const_int i32_t 1) "" builder|] in
              let mat = expr builder m in
              get_row mat row_num
      | SCall ("row_f", [m;r]) ->
              let row_num = [|L.build_add (expr builder r) (L.const_int i32_t 1) "" builder|] in
              let mat = expr builder m in
              get_row mat row_num
      | SCall ("dim_i", [m]) ->
              let mat = expr builder m in
              get_dim mat
      | SCall ("dim_f", [m]) ->
              let mat = expr builder m in
              get_dim mat
      | SCall ("col_i", [m;c]) ->
              L.build_call col_i_func [| expr builder m; expr builder c |] "col_i" builder
      | SCall ("col_f", [m;c]) ->
              L.build_call col_f_func [| expr builder m; expr builder c |] "col_f" builder
      | SCall ("spliceRow_i", [m;r]) ->
              L.build_call splice_row_i_func [| expr builder m; expr builder r |] "splice_row_i" builder
      | SCall ("spliceRow_f", [m;r]) ->
              L.build_call splice_row_f_func [| expr builder m; expr builder r |] "splice_row_f" builder
      | SCall ("spliceColumn_i", [m;r]) ->
              L.build_call splice_col_i_func [| expr builder m; expr builder r |] "splice_col_i" builder
      | SCall ("spliceColumn_f", [m;r]) ->
              L.build_call splice_col_f_func [| expr builder m; expr builder r |] "splice_col_f" builder
      | SCall ("switchRows_i", [m;r1;r2]) ->
              L.build_call switch_rows_i_func [| expr builder m; expr builder r1; expr builder r2 |] "switch_rows_i" builder
      | SCall ("switchRows_f", [m;r1;r2]) ->
              L.build_call switch_rows_f_func [| expr builder m; expr builder r1; expr builder r2 |] "switch_rows_f" builder
      | SCall ("size_i", [a]) ->
         let a' = expr builder a in
         get_size a'
      | SCall ("size_f", [a]) ->
         let a' = expr builder a in
         L.build_fptosi (get_size a') i32_t "" builder
      | SCall ("f_to_int", [e]) ->
         L.build_fptosi (expr builder e) i32_t "int_from" builder
      | SCall ("int_to_f", [e]) ->
         L.build_sitofp (expr builder e) float_t "float_from" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SFree(e) ->
         let e' = expr builder e in
         L.build_free e' builder
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
