open Lib
module SS = Set.Make(String);;

let print_ctx k v bo = "(" ^k ^", " ^ v ^ ");\n" ^ bo;;

let print_type a b = a ^ " | " ^  b;;
        let _ =
          let in_channel = open_in "input.pl" in
            let lexbuf = Lexing.from_channel in_channel in
            try 
                  let result = Parser.main Lexer.token lexbuf in
                    let newTypes = result |> Grammar.loop_types |> Grammar.print_types in 
                    result |> Grammar.loop_rules newTypes |> Grammar.print_rules;
                    print_newline();
                    flush stdout;
            with 
            Lexer.LexerException ->
              exit 0
            | Stdlib.Parsing.Parse_error -> 
              Lexer.print_error "error" "Parser" lexbuf ;
              exit 0
          