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

open Parsing;;
let _ = parse_error;;
# 4 "aspparse.mly"
open Ast
# 61 "aspparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* MODULO *);
  269 (* ASSIGN *);
  270 (* EXPAND *);
  271 (* INCREMENT *);
  272 (* DECREMENT *);
  273 (* ASSIGNPLUS *);
  274 (* ASSIGNMINUS *);
  275 (* ASSIGNTIMES *);
  276 (* ASSIGNDIVIDE *);
  277 (* ASSIGNMODULO *);
  278 (* NEG *);
  279 (* NOT *);
  280 (* EQ *);
  281 (* NEQ *);
  282 (* LT *);
  283 (* GT *);
  284 (* LEQ *);
  285 (* GEQ *);
  286 (* AND *);
  287 (* OR *);
  288 (* AT *);
  289 (* IF *);
  290 (* ELSE *);
  291 (* SWITCH *);
  292 (* SWAP *);
  293 (* CASE *);
  294 (* DEFAULT *);
  295 (* WHILE *);
  296 (* DO *);
  297 (* FOR *);
  298 (* INT *);
  299 (* BOOL *);
  300 (* FLOAT *);
  301 (* STRING *);
  302 (* RETURN *);
  303 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* ILIT *);
  305 (* FLIT *);
  306 (* BLIT *);
  307 (* ID *);
  308 (* SLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\004\000\007\000\
\007\000\009\000\009\000\008\000\008\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\011\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\017\000\017\000\017\000\017\000\
\017\000\018\000\018\000\018\000\019\000\019\000\020\000\020\000\
\020\000\020\000\021\000\021\000\021\000\022\000\022\000\022\000\
\022\000\022\000\023\000\023\000\023\000\024\000\024\000\025\000\
\025\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\016\000\016\000\026\000\026\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\002\000\002\000\002\000\008\000\000\000\
\001\000\001\000\003\000\000\000\002\000\002\000\003\000\003\000\
\001\000\001\000\005\000\007\000\005\000\005\000\005\000\002\000\
\005\000\006\000\009\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\005\000\003\000\001\000\002\000\002\000\003\000\
\004\000\001\000\002\000\002\000\001\000\004\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\085\000\000\000\
\000\000\000\000\000\000\012\000\013\000\014\000\001\000\000\000\
\000\000\004\000\007\000\003\000\000\000\000\000\017\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\000\038\000\039\000\000\000\
\040\000\000\000\000\000\000\000\025\000\026\000\036\000\045\000\
\000\000\000\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\082\000\
\000\000\051\000\052\000\000\000\000\000\000\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\015\000\021\000\022\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\044\000\
\023\000\000\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\041\000\048\000\
\075\000\076\000\077\000\078\000\079\000\080\000\053\000\056\000\
\057\000\058\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\084\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\043\000\
\000\000\029\000\030\000\031\000\033\000\000\000\000\000\000\000\
\034\000\000\000\028\000\000\000\000\000\035\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\029\000\011\000\022\000\050\000\
\023\000\051\000\052\000\053\000\054\000\055\000\056\000\071\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\072\000"

let yysindex = "\036\000\
\088\255\000\000\039\255\043\255\085\255\000\000\000\000\106\000\
\028\255\088\255\061\255\000\000\000\000\000\000\000\000\088\255\
\088\255\000\000\000\000\000\000\075\255\135\255\000\000\088\255\
\136\255\000\000\088\255\152\255\190\255\088\255\013\255\190\255\
\195\255\201\255\201\255\159\255\161\255\162\255\164\255\190\255\
\171\255\190\255\184\255\195\255\000\000\000\000\000\000\191\255\
\000\000\193\255\190\255\194\255\000\000\000\000\000\000\000\000\
\250\254\081\255\000\000\098\255\074\255\155\255\248\254\170\255\
\174\255\000\000\199\255\203\255\204\255\249\254\205\255\000\000\
\195\255\000\000\000\000\195\255\195\255\195\255\195\255\000\000\
\195\255\169\255\195\255\210\255\195\255\000\000\000\000\000\000\
\000\000\000\000\037\255\195\255\195\255\195\255\195\255\195\255\
\195\255\195\255\195\255\195\255\195\255\195\255\195\255\195\255\
\195\255\195\255\195\255\195\255\195\255\195\255\195\255\000\000\
\000\000\172\255\195\255\000\000\216\255\218\255\219\255\229\255\
\231\255\233\255\232\255\000\000\208\255\234\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\098\255\098\255\074\255\074\255\074\255\074\255\
\155\255\155\255\248\254\170\255\000\000\241\255\000\000\190\255\
\190\255\190\255\190\255\190\255\195\255\195\255\000\000\000\000\
\223\255\000\000\000\000\000\000\000\000\255\255\002\000\190\255\
\000\000\195\255\000\000\016\000\190\255\000\000"

let yyrindex = "\000\000\
\020\001\000\000\008\255\009\255\015\255\000\000\000\000\000\000\
\000\000\020\001\000\000\000\000\000\000\000\000\000\000\020\001\
\028\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\119\255\000\000\040\000\119\255\000\000\040\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\253\255\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\029\000\060\000\000\000\091\000\184\000\014\001\089\255\020\255\
\090\255\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\122\000\153\000\192\000\223\000\231\000\006\001\
\040\001\048\001\045\001\104\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\139\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\011\000\000\000\022\001\031\001\000\000\230\255\
\041\001\217\255\225\255\000\000\000\000\241\000\229\000\237\000\
\000\000\226\255\189\255\045\000\111\000\042\000\220\000\221\000\
\000\000\224\000"

let yytablesize = 607
let yytable = "\068\000\
\080\000\070\000\082\000\074\000\075\000\069\000\114\000\018\000\
\089\000\090\000\008\000\009\000\084\000\020\000\031\000\107\000\
\108\000\010\000\033\000\083\000\072\000\034\000\072\000\083\000\
\087\000\091\000\072\000\021\000\016\000\017\000\136\000\137\000\
\138\000\072\000\021\000\035\000\001\000\028\000\073\000\115\000\
\028\000\068\000\033\000\149\000\117\000\118\000\119\000\120\000\
\012\000\121\000\072\000\123\000\013\000\125\000\003\000\004\000\
\005\000\006\000\008\000\009\000\045\000\046\000\047\000\048\000\
\049\000\010\000\072\000\135\000\135\000\135\000\135\000\135\000\
\135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
\135\000\101\000\102\000\125\000\045\000\046\000\047\000\127\000\
\049\000\070\000\074\000\070\000\074\000\092\000\014\000\070\000\
\074\000\093\000\094\000\095\000\096\000\097\000\070\000\074\000\
\073\000\015\000\073\000\098\000\099\000\100\000\073\000\019\000\
\161\000\162\000\163\000\164\000\165\000\073\000\070\000\070\000\
\005\000\024\000\005\000\005\000\005\000\166\000\167\000\005\000\
\171\000\003\000\004\000\005\000\006\000\174\000\073\000\070\000\
\074\000\025\000\172\000\027\000\027\000\005\000\027\000\027\000\
\027\000\139\000\140\000\027\000\145\000\146\000\073\000\005\000\
\030\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\076\000\027\000\077\000\078\000\005\000\079\000\005\000\005\000\
\005\000\005\000\005\000\027\000\081\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\103\000\104\000\105\000\106\000\
\027\000\083\000\027\000\027\000\027\000\027\000\027\000\031\000\
\085\000\032\000\088\000\033\000\031\000\086\000\034\000\109\000\
\033\000\111\000\073\000\034\000\110\000\112\000\033\000\122\000\
\113\000\034\000\124\000\116\000\035\000\141\000\142\000\143\000\
\144\000\035\000\152\000\150\000\153\000\154\000\036\000\035\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\155\000\
\158\000\156\000\157\000\044\000\159\000\045\000\046\000\047\000\
\048\000\049\000\045\000\046\000\047\000\048\000\049\000\160\000\
\045\000\046\000\047\000\048\000\049\000\041\000\115\000\041\000\
\168\000\169\000\170\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\173\000\002\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\050\000\016\000\050\000\
\018\000\081\000\081\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\041\000\020\000\050\000\050\000\050\000\
\050\000\050\000\083\000\066\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\053\000\067\000\053\000\128\000\
\026\000\126\000\053\000\053\000\053\000\053\000\053\000\053\000\
\147\000\053\000\148\000\050\000\129\000\130\000\131\000\132\000\
\133\000\134\000\151\000\053\000\053\000\053\000\053\000\053\000\
\053\000\053\000\053\000\059\000\000\000\059\000\000\000\000\000\
\000\000\059\000\059\000\059\000\000\000\000\000\000\000\000\000\
\059\000\000\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000\060\000\000\000\060\000\000\000\000\000\000\000\
\060\000\060\000\060\000\000\000\000\000\000\000\000\000\060\000\
\000\000\059\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\061\000\000\000\061\000\000\000\000\000\000\000\061\000\
\061\000\061\000\000\000\000\000\000\000\000\000\061\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
\062\000\000\000\062\000\000\000\000\000\000\000\062\000\000\000\
\063\000\000\000\063\000\000\000\000\000\062\000\063\000\061\000\
\000\000\000\000\000\000\000\000\000\000\063\000\000\000\062\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\063\000\
\063\000\063\000\063\000\063\000\063\000\063\000\063\000\064\000\
\000\000\064\000\000\000\000\000\000\000\064\000\062\000\065\000\
\000\000\065\000\000\000\000\000\064\000\065\000\063\000\000\000\
\000\000\000\000\000\000\000\000\065\000\000\000\064\000\064\000\
\064\000\064\000\064\000\064\000\064\000\064\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\066\000\000\000\
\066\000\000\000\000\000\000\000\066\000\064\000\067\000\000\000\
\067\000\000\000\000\000\066\000\067\000\065\000\000\000\000\000\
\000\000\000\000\000\000\067\000\000\000\066\000\066\000\066\000\
\066\000\066\000\066\000\066\000\066\000\067\000\067\000\000\000\
\068\000\000\000\068\000\067\000\067\000\071\000\068\000\071\000\
\069\000\000\000\069\000\071\000\066\000\068\000\069\000\000\000\
\000\000\000\000\071\000\000\000\067\000\069\000\000\000\068\000\
\068\000\000\000\000\000\000\000\000\000\068\000\068\000\069\000\
\069\000\000\000\071\000\071\000\000\000\069\000\069\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\068\000\000\000\
\000\000\000\000\000\000\071\000\000\000\000\000\069\000"

let yycheck = "\031\000\
\040\000\033\000\042\000\034\000\035\000\032\000\014\001\010\000\
\015\001\016\001\003\001\003\001\044\000\016\000\002\001\024\001\
\025\001\003\001\006\001\003\001\001\001\009\001\003\001\007\001\
\051\000\032\001\007\001\017\000\001\001\002\001\098\000\099\000\
\100\000\014\001\024\000\023\001\001\000\027\000\002\001\047\001\
\030\000\073\000\006\001\111\000\076\000\077\000\078\000\079\000\
\010\001\081\000\031\001\083\000\010\001\085\000\042\001\043\001\
\044\001\045\001\051\001\051\001\048\001\049\001\050\001\051\001\
\052\001\051\001\047\001\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\109\000\110\000\
\111\000\008\001\009\001\115\000\048\001\049\001\050\001\051\001\
\052\001\001\001\001\001\003\001\003\001\013\001\010\001\007\001\
\007\001\017\001\018\001\019\001\020\001\021\001\014\001\014\001\
\001\001\000\000\003\001\010\001\011\001\012\001\007\001\051\001\
\152\000\153\000\154\000\155\000\156\000\014\001\030\001\031\001\
\002\001\047\001\004\001\005\001\006\001\157\000\158\000\009\001\
\168\000\042\001\043\001\044\001\045\001\173\000\031\001\047\001\
\047\001\003\001\170\000\004\001\002\001\023\001\004\001\005\001\
\006\001\101\000\102\000\009\001\107\000\108\000\047\001\033\001\
\001\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\002\001\023\001\002\001\002\001\046\001\002\001\048\001\049\001\
\050\001\051\001\052\001\033\001\002\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\026\001\027\001\028\001\029\001\
\046\001\002\001\048\001\049\001\050\001\051\001\052\001\002\001\
\002\001\004\001\001\001\006\001\002\001\005\001\009\001\030\001\
\006\001\003\001\002\001\009\001\031\001\003\001\006\001\039\001\
\005\001\009\001\001\001\007\001\023\001\103\000\104\000\105\000\
\106\000\023\001\003\001\048\001\003\001\003\001\033\001\023\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\003\001\
\001\001\003\001\002\001\046\001\003\001\048\001\049\001\050\001\
\051\001\052\001\048\001\049\001\050\001\051\001\052\001\007\001\
\048\001\049\001\050\001\051\001\052\001\001\001\047\001\003\001\
\034\001\003\001\001\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\003\001\000\000\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\001\001\003\001\003\001\
\003\001\007\001\003\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\047\001\005\001\017\001\018\001\019\001\
\020\001\021\001\007\001\030\000\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\001\001\031\000\003\001\091\000\
\024\000\085\000\007\001\008\001\009\001\010\001\011\001\012\001\
\109\000\014\001\110\000\047\001\092\000\093\000\094\000\095\000\
\096\000\097\000\115\000\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\001\001\255\255\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\255\255\255\255\255\255\255\255\
\014\001\255\255\047\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\001\001\255\255\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\255\255\255\255\255\255\255\255\014\001\
\255\255\047\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\001\001\255\255\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\014\001\255\255\
\047\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\001\001\255\255\003\001\255\255\255\255\255\255\007\001\255\255\
\001\001\255\255\003\001\255\255\255\255\014\001\007\001\047\001\
\255\255\255\255\255\255\255\255\255\255\014\001\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\001\001\
\255\255\003\001\255\255\255\255\255\255\007\001\047\001\001\001\
\255\255\003\001\255\255\255\255\014\001\007\001\047\001\255\255\
\255\255\255\255\255\255\255\255\014\001\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\001\001\255\255\
\003\001\255\255\255\255\255\255\007\001\047\001\001\001\255\255\
\003\001\255\255\255\255\014\001\007\001\047\001\255\255\255\255\
\255\255\255\255\255\255\014\001\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\024\001\025\001\255\255\
\001\001\255\255\003\001\030\001\031\001\001\001\007\001\003\001\
\001\001\255\255\003\001\007\001\047\001\014\001\007\001\255\255\
\255\255\255\255\014\001\255\255\047\001\014\001\255\255\024\001\
\025\001\255\255\255\255\255\255\255\255\030\001\031\001\024\001\
\025\001\255\255\030\001\031\001\255\255\030\001\031\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\047\001\255\255\
\255\255\255\255\255\255\047\001\255\255\255\255\047\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  ASSIGN\000\
  EXPAND\000\
  INCREMENT\000\
  DECREMENT\000\
  ASSIGNPLUS\000\
  ASSIGNMINUS\000\
  ASSIGNTIMES\000\
  ASSIGNDIVIDE\000\
  ASSIGNMODULO\000\
  NEG\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  AND\000\
  OR\000\
  AT\000\
  IF\000\
  ELSE\000\
  SWITCH\000\
  SWAP\000\
  CASE\000\
  DEFAULT\000\
  WHILE\000\
  DO\000\
  FOR\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  STRING\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  ILIT\000\
  FLIT\000\
  BLIT\000\
  ID\000\
  SLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 41 "aspparse.mly"
            ( _1)
# 454 "aspparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "aspparse.mly"
                 ( ([], [])               )
# 460 "aspparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 45 "aspparse.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 468 "aspparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 46 "aspparse.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 476 "aspparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "aspparse.mly"
              ( [] )
# 482 "aspparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 50 "aspparse.mly"
                           (  _1 :: _3 )
# 490 "aspparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "aspparse.mly"
         ( (_1, _2) )
# 498 "aspparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "aspparse.mly"
           ( Int   )
# 504 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "aspparse.mly"
           ( Bool  )
# 510 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "aspparse.mly"
           ( Float )
# 516 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "aspparse.mly"
           ( String)
# 522 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "aspparse.mly"
                ( Ptr(Int)   )
# 528 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "aspparse.mly"
                ( Ptr(Bool)  )
# 534 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "aspparse.mly"
                ( Ptr(Float) )
# 540 "aspparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 68 "aspparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 558 "aspparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "aspparse.mly"
              ( [] )
# 564 "aspparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 81 "aspparse.mly"
                 ( _1 )
# 571 "aspparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 84 "aspparse.mly"
        ( [_1] )
# 578 "aspparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 85 "aspparse.mly"
                             ( _1::_3 )
# 586 "aspparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "aspparse.mly"
                ( [] )
# 592 "aspparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 89 "aspparse.mly"
                    ( _1::_2 )
# 600 "aspparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "aspparse.mly"
                                            ( Expr _1      )
# 607 "aspparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "aspparse.mly"
                                            ( Block _2 )
# 614 "aspparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "aspparse.mly"
                                            ( Return _2      )
# 621 "aspparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_stmt) in
    Obj.repr(
# 95 "aspparse.mly"
                          ( _1 )
# 628 "aspparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_stmt) in
    Obj.repr(
# 96 "aspparse.mly"
                         ( _1 )
# 635 "aspparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "aspparse.mly"
                                           ( If(_3, _5, Block([])) )
# 643 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "aspparse.mly"
                                              ( If(_3, _5, _7) )
# 652 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "aspparse.mly"
                                            ( Switch(_3, _5) )
# 660 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "aspparse.mly"
                                   ( Swap(_3, _5) )
# 668 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "aspparse.mly"
                                            ( Case(_3, _5)   )
# 676 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "aspparse.mly"
                                            ( Default(_2)    )
# 683 "aspparse.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "aspparse.mly"
                                            ( While (_3, _5)  )
# 691 "aspparse.ml"
               : 'iteration_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "aspparse.mly"
                                            ( Do (_5, _2) )
# 699 "aspparse.ml"
               : 'iteration_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "aspparse.mly"
                                                    ( For(_3, _5, _7, _9) )
# 709 "aspparse.ml"
               : 'iteration_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 112 "aspparse.mly"
                         ( _1 )
# 716 "aspparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "aspparse.mly"
                             ( IntLit(_1)   )
# 723 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 116 "aspparse.mly"
                             ( FloatLit(_1) )
# 730 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 117 "aspparse.mly"
                             ( BoolLit(_1)  )
# 737 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "aspparse.mly"
              ( StringLit(_1))
# 744 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "aspparse.mly"
                             ( Id(_1)       )
# 751 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 120 "aspparse.mly"
                             ( List(_2)     )
# 758 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 121 "aspparse.mly"
                                    ( ListAlloc(_2, _4))
# 766 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "aspparse.mly"
                             ( _2           )
# 773 "aspparse.ml"
               : 'primary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expr) in
    Obj.repr(
# 125 "aspparse.mly"
                       ( _1 )
# 780 "aspparse.ml"
               : 'postfix_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'postfix_expr) in
    Obj.repr(
# 126 "aspparse.mly"
                                   ( UnPostop(_1, Incr) )
# 787 "aspparse.ml"
               : 'postfix_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'postfix_expr) in
    Obj.repr(
# 127 "aspparse.mly"
                                   ( UnPostop(_1, Decr) )
# 794 "aspparse.ml"
               : 'postfix_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'postfix_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expr) in
    Obj.repr(
# 128 "aspparse.mly"
                                   ( ListAccess(_1, _3) )
# 802 "aspparse.ml"
               : 'postfix_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 129 "aspparse.mly"
                                   ( Call (_1, _3)  )
# 810 "aspparse.ml"
               : 'postfix_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'postfix_expr) in
    Obj.repr(
# 134 "aspparse.mly"
                                   ( _1 )
# 817 "aspparse.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 135 "aspparse.mly"
                                   ( UnPreop(Neg, _2) )
# 824 "aspparse.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 136 "aspparse.mly"
                                   ( UnPreop(Not, _2) )
# 831 "aspparse.ml"
               : 'unary_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expr) in
    Obj.repr(
# 139 "aspparse.mly"
                                ( _1 )
# 838 "aspparse.ml"
               : 'cast_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expr) in
    Obj.repr(
# 140 "aspparse.mly"
                                (Cast(_2, _4) )
# 846 "aspparse.ml"
               : 'cast_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expr) in
    Obj.repr(
# 144 "aspparse.mly"
                               ( _1 )
# 853 "aspparse.ml"
               : 'mult_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expr) in
    Obj.repr(
# 145 "aspparse.mly"
                               ( Binop(_1, Mul, _3) )
# 861 "aspparse.ml"
               : 'mult_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expr) in
    Obj.repr(
# 146 "aspparse.mly"
                               ( Binop(_1, Div, _3) )
# 869 "aspparse.ml"
               : 'mult_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expr) in
    Obj.repr(
# 147 "aspparse.mly"
                               ( Binop(_1, Mod, _3) )
# 877 "aspparse.ml"
               : 'mult_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expr) in
    Obj.repr(
# 150 "aspparse.mly"
                              ( _1 )
# 884 "aspparse.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expr) in
    Obj.repr(
# 151 "aspparse.mly"
                              ( Binop(_1, Add,   _3)   )
# 892 "aspparse.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expr) in
    Obj.repr(
# 152 "aspparse.mly"
                              ( Binop(_1, Sub,   _3)   )
# 900 "aspparse.ml"
               : 'add_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 155 "aspparse.mly"
                                 ( _1 )
# 907 "aspparse.ml"
               : 'relational_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 156 "aspparse.mly"
                                 ( Binop(_1, Less,  _3)   )
# 915 "aspparse.ml"
               : 'relational_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 157 "aspparse.mly"
                                 ( Binop(_1, Great,  _3)   )
# 923 "aspparse.ml"
               : 'relational_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 158 "aspparse.mly"
                                 ( Binop(_1, LessEqual,  _3)   )
# 931 "aspparse.ml"
               : 'relational_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expr) in
    Obj.repr(
# 159 "aspparse.mly"
                                 ( Binop(_1, GreatEqual,  _3)   )
# 939 "aspparse.ml"
               : 'relational_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expr) in
    Obj.repr(
# 163 "aspparse.mly"
                                      ( _1 )
# 946 "aspparse.ml"
               : 'equality_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expr) in
    Obj.repr(
# 164 "aspparse.mly"
                                      ( Binop(_1, Equal, _3)   )
# 954 "aspparse.ml"
               : 'equality_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expr) in
    Obj.repr(
# 165 "aspparse.mly"
                                      ( Binop(_1, Neq, _3)     )
# 962 "aspparse.ml"
               : 'equality_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expr) in
    Obj.repr(
# 169 "aspparse.mly"
                                       ( _1 )
# 969 "aspparse.ml"
               : 'logical_and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expr) in
    Obj.repr(
# 170 "aspparse.mly"
                                       ( Binop(_1, And,   _3)   )
# 977 "aspparse.ml"
               : 'logical_and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expr) in
    Obj.repr(
# 173 "aspparse.mly"
                                        ( _1 )
# 984 "aspparse.ml"
               : 'logical_or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_or_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expr) in
    Obj.repr(
# 174 "aspparse.mly"
                                        ( Binop(_1, Or,    _3)   )
# 992 "aspparse.ml"
               : 'logical_or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_or_expr) in
    Obj.repr(
# 177 "aspparse.mly"
                                            ( _1 )
# 999 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 178 "aspparse.mly"
                                            ( Assign(_1, _3)         )
# 1007 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 179 "aspparse.mly"
                                             ( AssignBinop(_1, Add, _3) )
# 1015 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 180 "aspparse.mly"
                                              ( AssignBinop(_1, Sub, _3) )
# 1023 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 181 "aspparse.mly"
                                              ( AssignBinop(_1, Mul, _3) )
# 1031 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 182 "aspparse.mly"
                                             ( AssignBinop(_1, Div, _3) )
# 1039 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expr) in
    Obj.repr(
# 183 "aspparse.mly"
                                             ( AssignBinop(_1, Mod, _3) )
# 1047 "aspparse.ml"
               : 'assignment_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "aspparse.mly"
              ( [] )
# 1053 "aspparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 189 "aspparse.mly"
         ( _1 )
# 1060 "aspparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "aspparse.mly"
        ( [_1] )
# 1067 "aspparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 193 "aspparse.mly"
                    ( _1::_3 )
# 1075 "aspparse.ml"
               : 'args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
