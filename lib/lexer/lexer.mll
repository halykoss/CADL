  (* File lexer.mll *)
        {
        open Parser        (* The type token is defined in parser.mli *)
        exception LexerException
        (* La riga a cui mi trovo*)
        let num_lines = ref 0
        (* Il carattere della riga a cui mi trovo*)
        let line_start = ref 0
        (*Escape del carattere \n*)
        let matchError s = match s with 
          "\n" -> "EOS"
        | _ -> s
        (* Stampa il messaggio di errore*)
        let print_error s typerr lexbuf = Printf.fprintf 
              stderr 
              "%s error:\n\n Unexpected \"%s\" (%d-%d) on line = %d \n" 
              typerr
              (if s = "error" then (lexbuf |> Lexing.lexeme |> matchError) else s) 
              ((lexbuf |> Lexing.lexeme_start) - !line_start) 
              ((lexbuf |> Lexing.lexeme_end) - !line_start) 
              !num_lines
        }

        let digit = ['0'-'9']
        let lower = ['a'-'z']
        let capital = ['A'-'Z']
        let id    = lower (lower|digit)*
        let keywords = capital lower (lower|digit)+
        let variable = capital (capital|digit)*

        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { incr num_lines;line_start := (lexbuf |> Lexing.lexeme_end);EOL }
          | ['@'] keywords as lxm { KEYWORD lxm }
          | id as lxm { MIN lxm }
          | keywords as lxm { TYPE lxm }
          | variable as lxm { CAP lxm }
          | '#'             { FUNCDEF }
          | '+'            { PLUS }
          | ":-"           { INFERENCE }
          | ','            { COMMA }
          | '*'            { STAR }
          | '/'            { DIV }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | eof            { EOF }
          | _ as lxm       {  (lxm |> Printf.sprintf "%c" |> print_error) "Lexer" lexbuf; raise LexerException }