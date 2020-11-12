
open Lib
open Generated
open Utilities
let rec printType v = match v with 
TypI -> "Int"
| TypBool -> "Bool"
| TypUnit -> "Unit"
| TypF(t1,t2) -> "" ^ printType t1 ^ " -> " ^ printType t2
| TypList t -> printType t ^ " list"
| TypTu ([]) -> ""
| TypTu (x::xs) -> "(" ^ List.fold_left (fun y x -> y ^ " * " ^ (printType x) ) (printType x) xs  ^ ")"
| TypRef t -> printType t ^ " ref"
;;

if Array.length Sys.argv - 1 > 0 then 
let in_channel = open_in (Sys.argv.(1)) in
let lexbuf = Lexing.from_channel in_channel in
      (* File calc.ml *)
      let _ =
            let result = Parser.exp Lexer.token lexbuf in
            printType (type_check (FunContext.create ()) result) |> print_endline; flush stdout in ()
else 
  ("Input file non dato " |> print_endline); flush stdout; ()