open Lib
open Printf
module SS = Set.Make(String);;

let print_ctx k v bo = "(" ^k ^", " ^ v ^ ");\n" ^ bo;;

let print_type a b = a ^ " | " ^  b;;
          try 
            let in_channel = open_in (Sys.argv.(1)) in
              let lexbuf = Lexing.from_channel in_channel in
                try 
                      let result = Parser.main Lexer.token lexbuf in
                        result |> Grammar.loop_rules |> Grammar.print_rules;
                        print_newline();
                        flush stdout;
                with 
                  Lexer.LexerException ->
                    exit 0
                  | Stdlib.Parsing.Parse_error -> 
                    Lexer.print_error "error" "Parser" lexbuf ;
                    exit 0
          with 
            _ -> fprintf stderr "Input file does not exist!\n"