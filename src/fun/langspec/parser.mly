%{
(* parser *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI
Improvements/fixes:
    - English comments
    - Better errors
*)
open Batteries
open FunSpecification.FunSpecification
%}

/* (* Token definition *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LIST
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <string> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
/* (* New tokes: operators *) */
%token COLON
%token ARROW
%token AST
%token LBRACKET
%token RBRACKET
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token CONS
%token POINTER
%token POINTER_ASS
%token LAMBDA
%token LEFTG
%token RIGHTG
%token ISNIL
%token HEAD TAIL
%token APPLY
%token REF
/* (* New tokens: types *) */
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_FLOAT
%token TYPE_UNIT
%token EOF

/* (* Operators priority and associativity *) */
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%right ARROW
%left COMMA POINTER ISNIL
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL POINTER_ASS
%left PLUS MINUS PLUS_DOT MINUS_DOT CONS HEAD TAIL
%left AST_DOT SLASH_DOT AST
%right prec_unary_minus
%left APPLY
%left DOT


/* (* Start symbol *) */
%type <unit FunSpecification.FunSpecification.term> exp
%type <FunSpecification.FunSpecification.res> type 
%start exp

%%

exp: /* (* simple expressions *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit () }
| BOOL
    { Bool($1, ()) }
| INT
    { Num($1, ()) }
| IDENT
    { Var($1, ()) }
| LAMBDA IDENT COLON type DOT exp  {Fun($2,$4,$6, ())}
| exp  APPLY exp { App($3,$1,()) }
| LET IDENT EQUAL exp IN exp {Let($2,$4,$6,())}
| LEFTG tup_list RIGHTG     { DeclTup($2,()) }
| LEFTG INT ARROW exp RIGHTG      { GetTup($2,$4,()) }
| LBRACKET type RBRACKET { Nil($2,()) }
| ISNIL LBRACKET type RBRACKET LPAREN exp RPAREN { IsNil($3,$6,()) }
| CONS LBRACKET type RBRACKET LPAREN exp COMMA exp RPAREN  { Cons($3,$6,$8,())}
| HEAD LBRACKET type RBRACKET LPAREN exp RPAREN { Head($3,$6,())}
| TAIL LBRACKET type RBRACKET LPAREN exp RPAREN { Tail($3,$6,())}
| AST exp               { Ref($2,())}
| POINTER exp           { Deref($2,())}
| exp POINTER_ASS exp   { PointerAss($1,$3,()) }
;;

tup_list:
|   /* lambda */    { [] }
|   exp             { [$1] }       
|   exp COMMA tup_list { $1::$3 }

type:
    TYPE_INT  { TypI }
|   TYPE_BOOL { TypBool }
|   TYPE_UNIT { TypUnit }
|   LBRACKET type RBRACKET { TypList $2 }
|   LPAREN type type_f RPAREN {TypF($2,$3)}
|   LEFTG type type_list RIGHTG { TypTu($2::$3) }
|   REF type { TypRef $2 }

type_list:
|   /* lambda */    { [] }
|   COMMA type type_list { $2::$3 }

type_f:
    ARROW type    { $2 }
|   ARROW type type_f { TypF($2,$3) }
