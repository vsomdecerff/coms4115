(* Ocamllex scanner for ASP *)

{ open Aspparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii =  ['\x20'-'\x5B' '\x5D'-'\x7E'] 

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
| '@'	   { AT }
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
| "^"	   { EXPAND }
| "and"    { AND }
| "or"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "switch" { SWITCH }
| "swap"   { SWAP 	}
| "case"   { CASE }
| "default"{ DEFAULT }
| "while"  { WHILE }
| "do"     { DO }
| "for"    { FOR }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT    }
| "bool"   { BOOL   }
| "float"  { FLOAT  }
| "string" { STRING }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as lem  { ILIT(int_of_string lem) }
| (digit+) ['.'] digit+ as lem { FLIT(float_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| '"' (escape | ascii)* '"' as lem { SLIT(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and single_comment = parse
  "\n" { token lexbuf }
| _    { single_comment lexbuf }
