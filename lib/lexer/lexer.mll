  (* File lexer.mll *)
        {
        open Parser        (* The type token is defined in parser.mli *)
        open Keywords
        exception LexerException
        (* Il carattere della riga a cui mi trovo*)
        let line_start = ref 0
        (* Conta le occorenze di un carattere in una stringa*)
        let countCharInString c s = 
          let rec counter idx = 
              if idx == (String.length s) then 0 else if c == s.[idx] then 1 + counter (idx + 1) else counter (idx + 1)
                in 
                  counter 0;;
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
        let id    = lower ('_'|lower|capital|digit)*
        let keywords = capital lower (lower|capital|digit)+
        let variable = capital (capital|digit)*
        let closedEmb = '*''#'
        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { incr num_lines;line_start := (lexbuf |> Lexing.lexeme_end);token lexbuf }
          | "@PrintType"   { PRINTTYPE }
          | "@CompatEnv"   { COMPATENV }
          | "and"          { AND }
          | ['@'] keywords as lxm { KEYWORD lxm }
          | id as lxm { MIN lxm }
          | keywords as lxm { TYPE lxm }
          | variable as lxm { CAP lxm }
          | '#''#' [ ^ '#''#' ]* '#''#' as lxm { num_lines := !num_lines + 1 + (countCharInString '\n' lxm);OCAMLEMBEDDED lxm }
          | '#'             { TYPEDEFZONE }
          | '+'            { PLUS }
          | '|'            { SEPARATOR }
          | "::="          { NEWTYPE }
          | ":-"           { INFERENCE }
          | ','            { COMMA }
          | '.'            { DOT (!num_lines)  }
          | '*'            { STAR }
          | '/'            { DIV }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | '['            { LLIST }
          | ']'            { RLIST }
          | eof            { EOF }
          | _ as lxm       {  (lxm |> Printf.sprintf "%c" |> print_error) "Lexer" lexbuf; raise LexerException }