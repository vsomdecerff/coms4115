(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnPreop of preop * sexpr 
  | SAssign of sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SList of sexpr list
  | SListAccess of sexpr * sexpr 
  | SCast of typ * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SDo of sexpr * sstmt 
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SIntLit(l) -> string_of_int l
      | SFloatLit(l) -> string_of_float l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SStringLit(l) -> l
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
	  | SUnPreop(o, e) ->
		string_of_preop o ^ " " ^ string_of_sexpr e 
      | SAssign(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
      | SList(el) ->  "[ " ^ String.concat ", " (List.map string_of_sexpr el) ^ " ]"
      | SListAccess(l, i) ->  string_of_sexpr l ^ " @ " ^ string_of_sexpr i
  	  | SCast(t, e) -> "(" ^ string_of_typ t ^ ") " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SDo(e, s) -> "do " ^ string_of_sstmt s ^ "\nwhile ( " ^ string_of_sexpr e ^ " )"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
