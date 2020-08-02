open Lib
module SS = Set.Make(String);;

let head_text = "let matchHeadType tuple = let (exp,value) = tuple in match exp with
    Lambda(a, b) when a = value -> true
  | _ -> if exp = value then true else false;; \n\n";;

let tail_text =  "let matchTailType tuple = let (exp,value) = tuple in match exp with
  Lambda(a, b) when b = value -> true
| _ -> if exp = value then true else false;; \n\n";;

"let rec getFromContext ctx valueT = match ctx with 
(x,y)::xs -> let (key,value) = valueT in if x = key && y = value then true else getFromContext xs valueT
| [] -> false;;\n" |> print_endline;;

let print_ctx k v bo = "(" ^k ^", " ^ v ^ ");\n" ^ bo;;
let print_type a b = a ^ " | " ^  b;;
        let _ =
          let lexbuf = Lexing.from_channel stdin in
          try 
                let result = Parser.main Lexer.token lexbuf in
                  let (a,b) = Grammar.analyzeInput result in 
                  "type typed = " ^ (List.fold_right print_type (Hashtbl.find_all Grammar.newTypes "type") "Lambda of typed * typed | None;;\n")   |> print_endline;
                  "let ctx = [ \n" ^ (Hashtbl.fold print_ctx Grammar.ctx "];;\n") |> print_endline;
                  head_text |> print_endline;
                  tail_text |> print_endline;
                  SS.iter Grammar.iter_formulas a;
                  SS.iter Grammar.iter_rules b;
                    print_newline();
                    flush stdout; 
          with 
          Lexer.LexerException ->
            exit 0
          | Stdlib.Parsing.Parse_error -> 
            Lexer.print_error "error" "Parser" lexbuf ;
            exit 0
          