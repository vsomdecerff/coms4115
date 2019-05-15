/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN EXPAND
%token INCREMENT DECREMENT
%token ASSIGNPLUS ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE ASSIGNMODULO
%token NEG NOT EQ NEQ LT GT LEQ GEQ AND OR AT
%token IF ELSE SWITCH CASE DEFAULT WHILE DO FOR INT BOOL FLOAT 
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
%left EXPAND
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
  | INT TIMES   { Ptr(Int)   }
  | BOOL TIMES  { Ptr(Bool)  }
  | FLOAT TIMES { Ptr(Float) }

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
  | RETURN expr SEMI                        { Return $2      }
  | selection_stmt 							{ $1 }
  | iteration_stmt							{ $1 }

selection_stmt:
    IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | SWITCH LPAREN expr RPAREN stmt          { Switch($3, $5) }
  | CASE LPAREN expr RPAREN stmt            { Case($3, $5)   }
  | DEFAULT stmt                            { Default($2)    }

iteration_stmt:
    WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | DO stmt WHILE LPAREN expr RPAREN        { Do ($5, $2) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For($3, $5, $7, $9) }
  
expr:						
  assignment_expr 							{ $1 }

primary_expr:
  | ILIT                     { IntLit($1)   }
  | FLIT                     { FloatLit($1) }
  | BLIT                     { BoolLit($1)  }
  | ID                       { Id($1)       }
  | LBRACK args_opt RBRACK   { List($2)     }
  | LBRACK expr EXPAND ILIT RBRACK  { ListAlloc($2, $4)}
  | LPAREN expr RPAREN       { $2           }

postfix_expr:
    primary_expr				   { $1 }
  | postfix_expr INCREMENT         { UnPostop($1, Incr) }
  | postfix_expr DECREMENT         { UnPostop($1, Decr) }
  | postfix_expr AT primary_expr   { ListAccess($1, $3) }
  | ID LPAREN args_opt RPAREN      { Call ($1, $3)  }



unary_expr:
    postfix_expr                   { $1 }
  | MINUS unary_expr %prec NEG     { UnPreop(Neg, $2) }
  | NOT unary_expr %prec NOT       { UnPreop(Not, $2) }

cast_expr:
    unary_expr                  { $1 }
  | LPAREN typ RPAREN cast_expr {Cast($2, $4) }


mult_expr:
    cast_expr                  { $1 }
  | mult_expr TIMES  cast_expr { Binop($1, Mul, $3) }
  | mult_expr DIVIDE cast_expr { Binop($1, Div, $3) }
  | mult_expr MODULO cast_expr { Binop($1, Mod, $3) }

add_expr:
    mult_expr                 { $1 }
  | add_expr PLUS   mult_expr { Binop($1, Add,   $3)   }
  | add_expr MINUS  mult_expr { Binop($1, Sub,   $3)   }

relational_expr:
    add_expr                     { $1 }
  | relational_expr LT  add_expr { Binop($1, Less,  $3)   }
  | relational_expr GT  add_expr { Binop($1, Great,  $3)   }
  | relational_expr LEQ add_expr { Binop($1, LessEqual,  $3)   }
  | relational_expr GEQ add_expr { Binop($1, GreatEqual,  $3)   }


equality_expr:
    relational_expr                   { $1 }
  | equality_expr EQ  relational_expr { Binop($1, Equal, $3)   }
  | equality_expr NEQ relational_expr { Binop($1, Neq, $3)     }


logical_and_expr: 
    equality_expr                      { $1 }
  | logical_and_expr AND equality_expr { Binop($1, And,   $3)   }

logical_or_expr:
  logical_and_expr                      { $1 }
  | logical_or_expr OR logical_and_expr { Binop($1, Or,    $3)   }

assignment_expr:
    logical_or_expr			                      { $1 }
  | unary_expr ASSIGN assignment_expr   		  { Assign($1, $3)         }
  | unary_expr ASSIGNPLUS assignment_expr   	{ AssignBinop($1, Add, $3) }
  | unary_expr ASSIGNMINUS assignment_expr 	  { AssignBinop($1, Sub, $3) }
  | unary_expr ASSIGNTIMES assignment_expr 	  { AssignBinop($1, Mul, $3) }
  | unary_expr ASSIGNDIVIDE assignment_expr 	{ AssignBinop($1, Div, $3) }
  | unary_expr ASSIGNMODULO assignment_expr 	{ AssignBinop($1, Mod, $3) }


/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
