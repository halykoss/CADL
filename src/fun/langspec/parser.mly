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

let tresToString t = match t with
    "secret" -> Tsec
    | "public" -> Tpub
    | "any" -> Tany
    | _ -> failwith "Not a type!"
;;
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
%token SUCC
%token NIL
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <string> IDENT NAME
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
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LESS_MINUS
%token CASE
%token OF
%token SEMICOLON
%token LPAREN
%token PIPE
%token IS
%token AS
%token OUT
%token INP
%token RPAREN
/* (* New tokens: types *) */
%token TYPE_INT
%token REPLICATION
%token TYPE_BOOL
%token TYPE_FLOAT
%token TYPE_UNIT
%token LANG
%token RANG
%token EOF
/* (* Operators priority and associativity *) */
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%right ARROW
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT REPLICATION SLASH_DOT AST REPLICATION
%left PIPE CASE OF COLON
%right prec_unary_minus
%left prec_app
%left DOT
%left LANG RANG
%left OUT
%left LBRACE RBRACE
%left LBRACKET RBRACKET
%left LPAREN RPAREN


/* (* Start symbol *) */
%type <unit FunSpecification.FunSpecification.term> exp
%type <FunSpecification.FunSpecification.t> l
%start exp

%%

exp:
    NIL { Zero () }
|   LPAREN exp RPAREN { $2 }
|   LPAREN IDENT AS IDENT RPAREN IN exp { Restriction($2, tresToString $4, $7,()) }
|   exp PIPE exp { Parallel($1,$3,()) }
|   REPLICATION exp { Replication($2,()) }
|   LET LPAREN IDENT COMMA IDENT RPAREN EQUAL l IN exp { Splitting($3, $5, $8, $10, ()) }
|   LBRACKET l IS l RBRACKET exp { Match($2,$4,$6,()) }
|   CASE l OF NIL COLON exp SUCC LPAREN IDENT RPAREN COLON exp { IntCase($2,$6,$9,$12,()) }
|   CASE l OF LBRACE id_list RBRACE LPAREN l RPAREN IN exp { Skd($2, $5, $8,$11,()) }
|   l OUT LBRACKET l_list RBRACKET IN exp { Output($1,$4,$7,()) }
|   l INP LBRACKET id_list RBRACKET IN exp { Input($1,$4,$7,()) }
;

id_list:
    IDENT { $1::[] }
|   IDENT COMMA id_list { $1::$3 }
;

l:
    NIL { Zero }
|   SUCC LPAREN l RPAREN { Succ $3 }
|   LPAREN l COMMA l RPAREN { Pair($2,$4) }
|   NAME { Name $1 }
|   IDENT { Var $1 }
|   LBRACE l_list RBRACE LPAREN l RPAREN { Ske($2,$5) }
;

l_list:
    l { $1::[] }
|   l COMMA l_list { $1::$3 }
;;