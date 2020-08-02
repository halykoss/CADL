        %{
          open Grammar
        %}      
        %token <string> MIN CAP KEYWORD TYPE
        %token PLUS MINUS TIMES DIV
        %token LPAREN RPAREN
        %token INFERENCE
        %token COMMA
        %token FUNCDEF
        %token EOL
        %token EOF
        %token STAR
        %start main             /* the entry point */
        %type <Grammar.input> main
        %type <Grammar.atomList> atomList ls_atom
        %type <Grammar.param> star_param
        %%
        main:
            input EOF                { $1 }
        ;

        input:
            /* lambda */                    { None }
        |  atom EOL input                   { Formula($1,$3) }
        |  atom INFERENCE atomList EOL input{ Rule($1,$3,$5)}
        ;

        atomList:
        |   atom ls_atom                    { AtomList($1,$2) }
        ;

        ls_atom:
            /* lambda */                    { None }
        |   COMMA atom ls_atom              { AtomList($2,$3) }
        ;

        atom:
            MIN LPAREN paramList RPAREN     { Atom($1,$3) }
        |   KEYWORD LPAREN param COMMA param RPAREN { Keywords.keywords $1 $3 $5 }
        ;

        paramList:
            /* lambda */                    { None }
        |   param ls_param                  { ParamList($1,$2)}
        ;

        ls_param:
            /* lambda */                    { None }
        |   COMMA param ls_param              { ParamList($2,$3) }
        ;
        
        param:
            MIN                             { Name $1 }    
        |   CAP                             { Variable $1 }    
        |   TYPE                            { Type $1 }
        |   FUNCDEF param star_param        { Lambda($2,$3) }
        ;

        star_param:
            FUNCDEF                            { None }
        |   STAR param star_param            { Lambda($2,$3) }
        ;
