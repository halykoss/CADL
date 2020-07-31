  (* File lexer.mll *)
        {
        open Parser        (* The type token is defined in parser.mli *)
        open Keywords
        exception Eof
        }
        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { EOL }
          | ['@'] ['a'-'z']+ as lxm { keywords lxm }
          | ['a'-'z']+ as lxm { MIN lxm }
          | ['A'-'Z'] ['a'-'z''0'-'9']+ as lxm { TYPE lxm }
          | ['A'-'Z']+ as lxm { CAP lxm }
          | '#'             { FUNCDEF }
          | '+'            { PLUS }
          | ":-"           { INFERENCE }
          | ','            { COMMA }
          | '*'            { STAR }
          | '/'            { DIV }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | eof            { EOF }