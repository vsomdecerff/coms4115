(* Semantic checking for the ASP compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
	StringMap.add "printstring_" {
		rtyp = String;
        fname = "print";
        formals = [(String, "x")];
        locals = []; body = [] }  
		
		(StringMap.add "printfloat_" {
		rtyp = Float;
		fname = "print";
		formals = [(Float, "x")];
		locals = []; body = [] }
  
		(StringMap.add "printbool_" {
        rtyp = Bool;
        fname = "print";
        formals = [(Bool, "x")];
        locals = []; body = [] }

		(StringMap.add "printint_" {
		rtyp = Int;
		fname = "print";
		formals = [(Int, "x")];
		locals = []; body = [] } StringMap.empty) ) )
  in
  let rec get_built_types = function
    [] -> "_"
   | hd::tl -> (string_of_typ (fst hd)) ^ get_built_types tl
  in
  let rec add_built_func map fd =
	StringMap.add (fd.fname ^ (get_built_types fd.formals)) fd map
  in
  let function_decls = 
  (* Collect all function names into one symbol table *)
  	List.fold_left add_built_func built_in_decls functions
  in

  let check_func func =
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

	let rec add_func map fd =
    	let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    	and dup_err = "duplicate function " ^ fd.fname
    	and make_err er = raise (Failure er)
    	and n = fd.fname (* Name of the function *)
    	(* No duplicate functions or redefinitions of built-ins *)
    	in
    	if StringMap.mem n built_in_decls then make_err built_in_err
    	else
        	if StringMap.mem (n ^ (get_bind_types fd.formals)) map then
            	let mfd = StringMap.find (n ^ (get_bind_types fd.formals)) map in
            	if mfd.fname = n then make_err dup_err
            	else StringMap.add (n ^ (get_bind_types fd.formals)) fd map
    	else  StringMap.add (n ^ (get_bind_types fd.formals))  fd map
  	and

    (* Return a semantically-checked expression, i.e., with a type *)
    check_expr = function
        IntLit l -> (Int, SIntLit l)
	  | FloatLit l -> (Float, SFloatLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Id var -> (type_of_identifier var, SId var)
	  | AssignBinop(var, op, e) -> check_expr (Assign(var, Binop(var, op, e)))
	  | UnPostop(var, op) -> let op' = match op with 
							   Incr -> Add 
							 | Decr -> Sub 
						   in check_expr (Assign(var, (Binop(var, op', IntLit(1)))))

 	  | UnPreop(op, e) -> let sexpr' = match op with
							Not -> (Bool, SUnPreop(op, (check_bool_expr e)))
						  | Neg -> (Int, SUnPreop(op, (check_expr e)))
						in sexpr'
      | Assign(Id(var) , e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
		if lt = Float &&  rt = Int then 
			(lt, SAssign((lt, SId(var)), (rt, e')))
		else 
        (check_assign lt rt err, SAssign((lt, SId(var)), (rt, e')))

	  | Assign(ListAccess(l, i), e) as ex ->
			let get_first_type = function
                  [] -> raise (Failure "Cannot assign value to empty list")
            | hd :: tl -> fst (check_expr hd)
			in 
			let (rt, re') = check_expr e in
			let (lt, le') = (check_expr (ListAccess(l, i))) in
			let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
			in 
			((check_assign lt rt err) , SAssign( (lt, le') , (rt, re')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Mul | Div | Mod when t1 != Bool -> t1
            | Equal | Neq when t1 != Float -> Bool
            | Less | Great | LessEqual | GreatEqual  when t1 != Bool -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else if t1 = Float || t2 = Float then
			let t = match op with 
				Add | Sub | Mul | Div | Mod when t1 = Float || t2 = Float -> Float
			  | Less | Great when t1 != Bool && t2 != Bool  -> Bool
              | _ -> raise (Failure err)
		  in
		  (t, SBinop((t1, e1'), op, (t2, e2')))
		else raise (Failure err)
      | Call(fname, args) as call ->
		let var_list = get_ast_types args in
        let fd = (find_func (fname ^ var_list)) in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
 	  | List(el) ->
			let rec check_if_type ty = function 
				  [] -> []
				| e :: tl -> let(t, e') = check_expr e in 
					if ty = t then (t, e') :: check_if_type ty tl
					else raise (Failure ("All elements must be of the same type " ^ 
									string_of_typ ty ^ ", encountered " ^
									string_of_sexpr (t, e') ^ " of type " ^ string_of_typ t))
			in 
			let get_first_type = function 
				  [] -> Int 
			| hd :: tl -> fst (check_expr hd)
		in
		let f_ty = get_first_type el in 
		let el' = check_if_type f_ty el 
		in (Ptr(f_ty), SList(el'))

	  | ListAlloc(e, i) -> 
			let rec zeros n e l = 
				if n = 0 then l 
				else (zeros (n-1) e (e::l))
			in 
			let el = (zeros i e []) in 
			check_expr (List(el))

      | ListAccess(l, i) -> 
			let get_el_type = function 
				Ptr(f_ty) -> f_ty
			in 
			let sl = check_expr l in
			let ty = get_el_type (fst sl) in
		 (ty , SListAccess(sl , (check_expr i)))

      | Cast(t, e) -> 
		 (t, SCast(t, check_expr e))
      | _ as unk -> raise (Failure ("Unable to semantically parse " ^ string_of_expr unk ))

   and 
	check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
   and
	get_bind_types = function
    [] -> "_"
        | hd::tl -> (string_of_typ (fst hd)) ^ get_bind_types tl
  and
	get_ast_types = function
    [] -> "_"
        | hd::tl -> let (et, e') = check_expr hd
          in (string_of_typ et) ^ get_ast_types tl
  and 
  (* Return a function from our symbol table *)
  find_func s =
    let rec print_users = function
    	[] -> "" 
		| hd::tl  -> (fst hd) ^ "\n" ^ print_users tl
	in 
	try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s ^ " " ^ string_of_int (StringMap.cardinal function_decls) ^ print_users (StringMap.bindings function_decls)))
  in

	let rec check_nested_const_case_list e sl =
      match sl with
      | [] -> Block([])
	  | Case(ce, cs) :: sl' -> (match ce with 
			IntLit(_) | BoolLit(_) | StringLit(_) -> If( (Binop(e, Equal, ce)) , cs, Block((check_nested_const_case_list e sl')::[])) 
			| _ -> raise (Failure ("Only const expressions in switch cases.  See swap"))
			)
	in 
	let rec check_multi_case_list e sl = 
	  match sl with 
	  | [] -> []
	  | Case(ce, cs) :: Default(ds) :: [] -> 
			If( (Binop(e, Equal, ce)) , cs, ds) :: (check_multi_case_list e [])
	  | Case(ce, cs) :: sl' -> If( (Binop(e, Equal, ce)) , cs, Block([])) :: (check_multi_case_list e sl')
	in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)

    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)

	 | Switch(e, Block sl) -> 
			(match e with 
			IntLit(_) | BoolLit(_) | FloatLit(_) | Id(_) ->  check_stmt (check_nested_const_case_list e sl)
			| _ -> raise (Failure ("Can only use constant expressions in switch.  See swap"))
		)
	  | Swap(e, Block sl) ->
			check_stmt (Block(check_multi_case_list e sl))

      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
	  | For(e1, e2, e3, Block sl) ->
		check_stmt (Block (Expr e1 :: While(e2, Block (List.rev (Expr e3 :: List.rev sl ))) :: []))
	  | Do(e, st) ->
		SDo(check_bool_expr e, check_stmt st)

      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)
