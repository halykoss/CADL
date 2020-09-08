open Lib
open Printf
module SS = Set.Make(String);;

          try
            let in_channel = open_in (Sys.argv.(1)) in
              let lexbuf = Lexing.from_channel in_channel in
                (try
                      let result = Parser.main Lexer.token lexbuf in
                        try 
                          if  (compare Sys.argv.(2) "inc") == 0 then 
                            (
                              "module FunSpecification (* : LanguageSpecification *) = struct" |> print_endline;
                              let _ = result |> Grammar.loop_rules_incremental in
                               "end" |> print_endline;
                                ()
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
          with
            _ -> fprintf stderr "Input file does not exist!\n"