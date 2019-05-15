type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | ASSIGN
  | EXPAND
  | INCREMENT
  | DECREMENT
  | ASSIGNPLUS
  | ASSIGNMINUS
  | ASSIGNTIMES
  | ASSIGNDIVIDE
  | ASSIGNMODULO
  | NEG
  | NOT
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | AT
  | IF
  | ELSE
  | SWITCH
  | SWAP
  | CASE
  | DEFAULT
  | WHILE
  | DO
  | FOR
  | INT
  | BOOL
  | FLOAT
  | STRING
  | RETURN
  | COMMA
  | ILIT of (int)
  | FLIT of (float)
  | BLIT of (bool)
  | ID of (string)
  | SLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
