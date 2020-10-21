{
(* lexer *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI
Improvements/fixes:
    - English comments
    - Better errors
*)
open Batteries

open FunSpecification
open Parser
}

(* Shorthands for frenquently used regexes *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

(* Actual rules *)
rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| "let"
    { LET }
| "in"
    { IN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "rec"
    { REC }
| '+'
    { PLUS }
| '-'
    { MINUS }
| ','
    { COMMA }
| '='
    { EQUAL }
| '_'
    { IDENT(Id.gentmp FunSpecification.TypUnit) }
| "List.create" | "List.make" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| ":"
    { COLON }
| "->"
    { ARROW }
| "*"
    { AST }
| "!"
    { POINTER }
| ":="
    { POINTER_ASS }
| "isNil"
    { ISNIL }
| "cons"
    { CONS }
| "head"
    { HEAD }
| "tail"
    { TAIL }
| "list"
     { LIST }
| "int"
    { TYPE_INT }
| "bool"
    { TYPE_BOOL }
| "unit"
    { TYPE_UNIT }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| "->"
    { ARROW }
| "\\"
    { LAMBDA }
| "|>"
    { APPLY }
| "{"
    {LEFTG }
| "}"
    { RIGHTG }
| "ref"
    { REF }
| lower (digit|lower|upper|'_')* (* Identifiers: start with lower case letters, then continue with 0 or more chars *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
        (Printf.sprintf "Syntax Error: unknown token %s @ %d:%d"
           (Lexing.lexeme lexbuf)
           ((Lexing.lexeme_start_p lexbuf).pos_lnum)
           ((Lexing.lexeme_start_p lexbuf).pos_cnum)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "Warning: unterminated comment!" }
| _
    { comment lexbuf }
