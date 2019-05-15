(* IR generation: translate takes a semantically checked AST and
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
  let the_module = L.create_module context "ASP" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and flt_t 	 = L.double_type  context 
  and i8_t       = L.i8_type     context
  and str_t 	 = L.pointer_type (L.i8_type context)
  and i1_t       = L.i1_type     context in

  (* Return the LLVM type for a ASP type *)
  let rec ltype_of_typ = function
      A.Int     -> i32_t
    | A.Float   -> flt_t
    | A.Bool    -> i1_t
    | A.String  -> str_t
    | A.Ptr(t)  -> L.pointer_type (ltype_of_typ t)
  in
  let rec string_of_Atyp = function
      A.Int     -> "int"
    | A.Float   -> "float"
    | A.Bool    -> "bool"
    | A.String  -> "string"
    | A.Ptr(t)  -> (string_of_Atyp t) ^"*"
  in


  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in 

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let rec get_sformal_types = function 
	[] -> "_"
	| hd::tl -> (string_of_Atyp (fst hd)) ^ get_sformal_types tl
	in 
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
		let var_list = get_sformal_types fdecl.sformals in 
      StringMap.add (name^var_list) (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let rec print_users = function
        [] -> ""
        | hd::tl  -> (fst hd) ^ "\n" ^ print_users tl
    in
  let build_function_body fdecl =
    if false = (StringMap.mem (fdecl.sfname^get_sformal_types fdecl.sformals) function_decls) then  raise (Failure( "VS" ^ fdecl.sfname^get_sformal_types fdecl.sformals ^ "\n" ^ print_users (StringMap.bindings function_decls)))
   else
	 let (the_function, _) = StringMap.find (fdecl.sfname^get_sformal_types fdecl.sformals) function_decls in
	 let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
	let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in 
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

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
      with Not_found ->  StringMap.find n global_vars 
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((t_, e) : sexpr) = match e with
        SIntLit i     -> L.const_int i32_t i
      | SFloatLit f   -> L.const_float flt_t f
      | SBoolLit b    -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s  -> L.build_global_stringptr s "str" builder
      | SId s         -> L.build_load (lookup s) s builder
      | SAssign ( (lt, SId(s)), e) -> 
			let e' = build_expr builder e in
			let rt = fst e in
			if lt = A.Float && rt = A.Int then
				let ce' = L.build_sitofp (e') flt_t "cstf" builder in
				 ignore (L.build_store ce' (lookup s) builder); ce'
			else let nop = 0 in
				ignore (L.build_store e' (lookup s)  builder); e'
	  | SAssign ( (lt, SListAccess(l, i)), e) -> let e' = build_expr builder e in
		    let idx = build_expr builder i  in
            let idx = L.build_add idx (L.const_int i32_t 0) "access1" builder in
            let arr = build_expr builder l in
            let res = L.build_gep arr [| idx |] "access2" builder in
		ignore (L.build_store e' res builder); e'

	  | SCast (t, e) ->
		let te = fst e in 
		let e' = build_expr builder e in 
		if t = te then e'
		else if t = A.Float then L.build_sitofp (e') flt_t "cstf" builder
		else if t = A.Bool then 
				if te = A.Int then L.build_trunc (e') i1_t "cstb" builder		
				else L.build_fptoui (e') i1_t "cstb" builder
		else (* t = A.Int *)
				if te = A.Bool then L.build_sext (e') i32_t "csti" builder
				else L.build_fptoui (e') i32_t "csti" builder

	  | SBinop (e1, op, e2) ->
		let t1 = fst e1 in 
		let t2 = fst e2 in 
		let e1' = (if (t_ = A.Float && t1 != A.Float) || (t_ = A.Bool && t1 != A.Float && t2 = A.Float)
				   then L.build_sitofp (build_expr builder e1) flt_t "cst1" builder
				   else build_expr builder e1)
		in 
		let e2' = (if (t_ = A.Float && t2 != A.Float) || (t_ = A.Bool && t2 != A.Float && t1 = A.Float)
				   then L.build_sitofp (build_expr builder e2) flt_t "cst2" builder
				   else build_expr builder e2)
		in
		if t_ = A.Int || (t2 = A.Int && t1 = A.Int) || (t2 = A.Bool && t1 = A.Bool)then
			(match op with
			   A.Add     -> L.build_add
			 | A.Sub     -> L.build_sub
			 | A.Mul 	 -> L.build_mul
			 | A.Div     -> L.build_sdiv
			 | A.Mod 	 -> L.build_srem 
			 | A.And     -> L.build_and
			 | A.Or      -> L.build_or
			 | A.Equal   -> L.build_icmp L.Icmp.Eq
			 | A.Neq     -> L.build_icmp L.Icmp.Ne
			 | A.Less    -> L.build_icmp L.Icmp.Slt
			 | A.Great   -> L.build_icmp L.Icmp.Sgt
			 | A.LessEqual    -> L.build_icmp L.Icmp.Sle
			 | A.GreatEqual   -> L.build_icmp L.Icmp.Sge
			) e1' e2' "tmp" builder
		else 
			(match op with
               A.Add     -> L.build_fadd
             | A.Sub     -> L.build_fsub
             | A.Mul     -> L.build_fmul
             | A.Div     -> L.build_fdiv
             | A.Less    -> L.build_fcmp L.Fcmp.Olt
             | A.Great   -> L.build_fcmp L.Fcmp.Ogt
            ) e1' e2' "tmp" builder
	  | SUnPreop(op, e) -> 
		let e' = build_expr builder e in
    	(match op with
           A.Not     -> L.build_not
         | A.Neg     -> L.build_neg
        ) e' "tmp" builder
	  | SList(el) -> 
			let sizeva = (List.length el) in
			let size = L.const_int i32_t sizeva in
			let ty = ltype_of_typ t_ in
			let arr = L.build_array_malloc ty size "init1" builder in
			let arr = L.build_pointercast arr ty "init2" builder in
			let _ = L.build_bitcast size ty "init3" builder in
			let values = List.map (build_expr builder) el in
			let buildf i v =
			(let arr_ptr =
			L.build_gep arr [| (L.const_int i32_t (i)) |] "init4" builder in
			ignore(L.build_store v arr_ptr builder);)
		in
		List.iteri buildf values; arr
		(*L.const_array (ltype_of_typ t_) (Array.of_list  (List.map (build_expr builder) el ))*)
	  | SListAccess(l, i) ->
			let idx = build_expr builder i  in
			let idx = L.build_add idx (L.const_int i32_t 0) "access1" builder in
			let arr = build_expr builder l in
			let res = L.build_gep arr [| idx |] "access2" builder in
		L.build_load res "access3" builder

	  | SCall ("print", [e]) when "print"^get_sformal_types[e] = "printint_" || "print"^get_sformal_types[e] = "printbool_" ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
	  | SCall ("print", [e]) when "print"^get_sformal_types[e] = "printfloat_" ->
        L.build_call printf_func [| float_format_str ; (build_expr builder e) |]
          "printf" builder 
	  | SCall ("print", [e]) when "print"^get_sformal_types[e] = "printstring_" ->
        L.build_call printf_func [| string_format_str ; (build_expr builder e) |]
          "printf" builder

      | SCall (f, args) ->
		try 
        let (fdef, fdecl) = StringMap.find (f^get_sformal_types args) function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
		with Not_found -> 
		raise (Failure ("Call" ^  (f^get_sformal_types args) ^ print_users (StringMap.bindings global_vars)))
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
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

		let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (build_stmt (L.builder_at_end context then_bb) then_stmt) (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        add_terminal (build_stmt (L.builder_at_end context else_bb) else_stmt) (L.build_br merge_bb);

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

	  | SDo (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let while_builder = L.builder_at_end context while_bb in
        let build_br_while = L.build_br while_bb in (* partial function *)
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let build_br_body = L.build_br body_bb in (* partial function *)
        ignore (build_br_body builder);
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
