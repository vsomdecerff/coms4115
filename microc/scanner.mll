(* Ocamllex scanner for MicroC *)

{ open Microcparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//" 	   { single_comment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['	   { LBRACK }
| ']'	   { RBRACK }
| ';'      { SEMI }
(* COMMA *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'	   { DIVIDE }
| '%'	   { MODULO }
| '='      { ASSIGN }
| "+="	   { ASSIGNPLUS }
| "-="     { ASSIGNMINUS }
| "*="     { ASSIGNTIMES }
| "/="     { ASSIGNDIVIDE }
| "%="     { ASSIGNMODULO }
| "++"	   { INCREMENT }
| "--"     { DECREMENT }
| "not"    { NOT }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>' 	   { GT }
| ">=" 	   { GEQ }
| "<="	   { LEQ }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "switch" { SWITCH }
| "case"   { CASE }
| "default"{ DEFAULT }
| "while"  { WHILE }
| "do"     { DO }
| "for"    { FOR }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and single_comment = parse
  "\n" { token lexbuf }
| _    { single_comment lexbuf }
