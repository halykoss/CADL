        %{
          open Grammar
        %}      
        %token <string> MIN CAP KEYWORD TYPE OCAMLEMBEDDED FORCONTEXT
        %token PLUS MINUS TIMES DIV
        %token LPAREN RPAREN
        %token INFERENCE
        %token NEWTYPE
        %token SEPARATOR
        %token PRINTTYPE
        %token COMMA
        %token LLIST 
        %token EVERY
        %token RLIST
        %token COMPATENV
        %token <int> NUM
        %token <int> DOT
        %token AND
        %token TYPEDEFZONE
        %token EOL
        %token EOF
        %token STAR
        %start main             /* the entry point */
        %type <Grammar.input> main
        %type <Grammar.paramList>  ls_def_type
        %type <Grammar.atomList> atomList ls_atom 
        %%
        main:
             input EOF          { $1  }
        ;

        input:
            /* lambda */                                { None }
        |   OCAMLEMBEDDED input                         { OcamlEmbedded(String.sub $1 2 ((String.length $1) - 4),$2) }
        |   param NEWTYPE param ls_def_type next_type DOT input   { DeclarationType(Declaration($1,ParamList($3,$4),$5),$7) }
        |   atom DOT input                              { Formula($2,$1,$3) }
        |   COMPATENV OCAMLEMBEDDED input               { CompatEnv(String.sub $2 2 ((String.length $2) - 4),$3)}
        |   atom INFERENCE atomList DOT input           { Rule($4,$1,$3,$5)}
        ;

        next_type:
            /* lambda */                                    { EndDecl } 
        |   AND param NEWTYPE param ls_def_type next_type   { Declaration($2,ParamList($4,$5),$6) }
        ;

        atom:
            MIN LPAREN paramList RPAREN     { Atom($1,$3) }
        |   FORCONTEXT LPAREN paramList RPAREN { ForContext((String.sub $1 1 ((String.length $1) - 1)),$3) }
        |   KEYWORD LPAREN paramList RPAREN { Keywords.keywords (String.sub (String.lowercase_ascii ($1)) 1 ((String.length $1) - 1)) $3 }
        ;

        param:
            MIN                             { Name $1 }    
        |   CAP                             { Variable $1 }  
        |   EVERY                           { Every }
        |   NUM                             { Num $1 }
        |   TYPE                            { TypeS $1 }  
        |   TYPE LPAREN paramList RPAREN    { Type($1,$3) }
        |   LLIST MIN RLIST                 { List $2 }
        ;
        
        ls_def_type:
            /* lambda */                    { NoneP }
        |   SEPARATOR param ls_def_type     { ParamList($2,$3) }
        ;

        atomList:
        |   atom ls_atom                    { AtomList($1,$2) }
        ;

        ls_atom:
            /* lambda */                    { None }
        |   COMMA atom ls_atom              { AtomList($2,$3) }
        ;

        paramList:
            /* lambda */                    { NoneP }
        |   param ls_param                  { ParamList($1,$2) }
        ;

        ls_param:
            /* lambda */                    { NoneP }
        |   COMMA param ls_param              { ParamList($2,$3) }
        ;
