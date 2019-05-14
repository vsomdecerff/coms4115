/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN 
%token INCREMENT DECREMENT
%token ASSIGNPLUS ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE ASSIGNMODULO
%token NEG NOT EQ NEQ LT GT LEQ GEQ AND OR AT
%token IF ELSE SWITCH CASE DEFAULT WHILE DO FOR INT BOOL FLOAT INT_ BOOL_ FLOAT_
/* return, COMMA token */
%token RETURN COMMA
%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN ASSIGNPLUS ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE ASSIGNMODULO
%left OR
%left AND
%left EQ NEQ 
%left LT GT LEQ GEQ
%left AT
%left PLUS MINUS
%left INCREMENT DECREMENT
%left TIMES DIVIDE MODULO
%right NOT NEG

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT    { Int   }
  | BOOL   { Bool  }
  | FLOAT  { Float }
  | INT_   { Ptr(Int)   }
  | BOOL_  { Ptr(Bool)  }
  | FLOAT_ { Ptr(Float) }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | SWITCH LPAREN expr RPAREN stmt		 	{ Switch($3, $5) }
  | CASE LPAREN expr RPAREN stmt            { Case($3, $5)   }
  | DEFAULT stmt							{ Default($2)    }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | DO stmt WHILE LPAREN expr RPAREN 		{ Do ($5, $2) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For($3, $5, $7, $9) } 
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
  | ILIT   		     { IntLit($1)             }
  | FLIT			 { FloatLit($1)			  }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | LBRACK args_opt RBRACK 		{ List($2)   }
  | expr AT expr    			{ ListAccess($1, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MODULO expr { Binop($1, Mod, $3) }
  | expr INCREMENT   { UnPostop($1, Incr) }
  | expr DECREMENT   { UnPostop($1, Decr) }
  | MINUS expr %prec NEG     { UnPreop(Neg, $2) }
  | NOT expr %prec NOT 		 { UnPreop(Not, $2)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Great,  $3)   }
  | expr LEQ    expr { Binop($1, LessEqual,  $3)   }
  | expr GEQ    expr { Binop($1, GreatEqual,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr ASSIGN expr   { Assign($1, $3)         }
  | expr ASSIGNPLUS expr { AssignBinop($1, Add, $3) }
  | expr ASSIGNMINUS expr { AssignBinop($1, Sub, $3) }
  | expr ASSIGNTIMES expr { AssignBinop($1, Mul, $3) }
  | expr ASSIGNDIVIDE expr { AssignBinop($1, Div, $3) }
  | expr ASSIGNMODULO expr { AssignBinop($1, Mod, $3) }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
