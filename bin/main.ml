open Lib
module SS = Set.Make(String);;

let decl = "module VarSet = Set.Make(struct
	type t = string
	let compare = String.compare
end);;\n
module FunContext = Hashtbl.Make(struct
	type t = string
	let compare = String.compare
	(* let hash = Hashtbl.hash *)
	let hash = Hashtbl.hash_param max_int max_int
	let equal = String.equal
end);;\n";;

let compatEnv = "
let compat gamma gamma' at =
    (* Straightorward implementation from the theory: *)
    let fv = snd (term_getannot at) in
        VarSet.for_all (fun v -> (FunContext.find_opt gamma v) = (FunContext.find_opt gamma' v)) fv;;\n";;

let in_channel = open_in (Sys.argv.(1)) in
let lexbuf = Lexing.from_channel in_channel in
(try
   let result = Parser.main Lexer.token lexbuf in
   try 
     if  (compare Sys.argv.(2) "inc") == 0 then 
       (
         let evaluated = result |> Grammar.loop_rules_incremental in
         "module FunSpecification (* : LanguageSpecification *) = struct" |> print_endline;
         let _ = !(Grammar.decTerm) |> Grammar.print_term_getannot |> print_endline  in 
         let _ = !(Grammar.decTerm) |> Grammar.print_term_edit |> print_endline  in 
         let _ = "\n\tlet rec compute_hash e = Hashtbl.hash_param max_int max_int e;;" |> print_endline in
         let _ = !(Grammar.decTerm) |> Grammar.print_get_sorted_children |> print_endline  in 
         let _ = (if not (!(Grammar.isCompatEnvDec)) then compatEnv else "") |> print_endline in 
         let _ = (evaluated |> Grammar.printCheck_join) in
         let _ = (evaluated |> Grammar.printFreeVar) in
         let _ = (evaluated |> Grammar.printTr) in
         let _ = Grammar.Eval.remove "check" evaluated in 
         "end" |> print_endline
       )
     else
       result |> Grammar.loop_rules |> Grammar.print_rules
   with 
     Invalid_argument(_) -> result |> Grammar.loop_rules |> Grammar.print_rules
 with
 | Lexer.LexerException ->
   exit 0
 | Stdlib.Parsing.Parse_error ->
   Lexer.print_error "error" "Parser" lexbuf ;
   exit 0);
print_newline();
flush stdout;