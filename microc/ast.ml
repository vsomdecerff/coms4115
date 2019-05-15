(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mul | Div | Mod |  Equal | Neq | Less | Great | LessEqual | GreatEqual | And | Or 

type postop = Incr | Decr 

type preop = Not | Neg

type typ = Int | Bool | Float | String | Ptr of typ

type expr =
    IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | AssignBinop of expr * op * expr
  | Assign of expr * expr
  | UnPostop of expr * postop 
  | UnPreop of preop * expr
  (* function call *)
  | Call of string * expr list
  | List of expr list
  | ListAlloc of expr * int 
  | ListAccess of expr * expr
  | Cast of typ * expr 
	
type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Case of expr * stmt
  | Default of stmt 
  | Switch of expr * stmt
  | While of expr * stmt
  | Do of expr * stmt 
  | For of expr * expr * expr * stmt
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Ptr(t) -> string_of_typ t ^ " *"

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Great -> ">"
  | LessEqual -> "<="
  | GreatEqual -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_postop = function
    Incr -> "++"
  | Decr -> "--"

let string_of_preop = function
    Not -> "not"
  | Neg -> "-"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> string_of_expr v  ^ " = " ^ string_of_expr e
  | AssignBinop(v, o, e) -> string_of_expr v ^ " " ^ string_of_op o ^ "= " ^ string_of_expr e
  | UnPostop(v, o) -> string_of_expr v ^ string_of_postop o
  | UnPreop(o, e) -> string_of_preop o ^ " " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | List(el) -> "[ " ^ String.concat ", " (List.map string_of_expr el) ^ " ]"
  | ListAlloc(e, i) -> "[ " ^ string_of_expr e ^ " ^ " ^ string_of_int i ^ " ]" 
  | ListAccess(l, i) -> string_of_expr l ^ " @ " ^ string_of_expr i 
  | Cast(t, e) -> "(" ^ string_of_typ t ^ ") " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Switch(e, s) -> "switch (" ^ string_of_expr e ^  ") " ^ string_of_stmt s
  | Case(e, s) -> "case (" ^ string_of_expr e ^  ") " ^ string_of_stmt s
  | Default(s) -> "default " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Do(e, s) -> "do " ^ string_of_stmt s ^ "\nwhile ( " ^ string_of_expr e ^ " )" 
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ 
						   string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
