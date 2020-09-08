(** Parametro usato dentro le() delle regole e delle Formule *)
type param = Name of string | Variable of string | Type of string * paramList | TypeS of string
(** Lista di parametri *)
and paramList = ParamList of param * paramList | NoneP;;
(** Dichiarazione di nuovi tipi *)
type declarationType = Declaration of param * paramList * declarationType | EndDecl;;
(** Fomrula atomica *)
type atom = Atom of string * paramList | Keyword of string * paramList | Context of paramList | Add of paramList | Member of paramList;;
(** Lista di formule atomiche *)
type atomList = AtomList of atom * atomList | None;;

let contains_term = Hashtbl.create 123456;;

let nextContext = ref 0;;
let decTerm = ref (Declaration(Name "unit",NoneP,EndDecl));
(* Lista di input (Regole, Formule, Codice Ocaml Embedded, nuovi tipi ) *)
type input = 
  Rule of int * atom * atomList * input 
  | Formula of int * atom * input 
  | OcamlEmbedded of string * input 
  | DeclarationType of declarationType * input 
  | None;;

(* Valutazione delle righe contenenti Regole e formule *)
type new_evaluation =
  Predicate of {
    rows     : (paramList * atomList * int) list;
  } 
  | Function of {
    rows     : (paramList * atomList * param * int) list;
  };;

(* Map contenente la lista delle regole e formule valutate *)
module Eval =  Map.Make(String);;

(* Costruisce una stringa con tutti i nuovi tipi definiti dall'utente *)
let print_types t = 
    let rec print_paramList pm = (
        match pm with
        NoneP -> ""
        | ParamList(Name vl,nxt) -> vl ^ if nxt == NoneP then print_paramList nxt else " * " ^ print_paramList nxt
        | _ -> failwith("Type declaration error 1")
        )
   in
    let rec print_newType nt = (
      match nt with 
      NoneP -> ""
      | ParamList(TypeS a,nxt) -> "\n | " ^ a ^ print_newType nxt
      | ParamList(Type(a,b),nxt) -> "\n | " ^ a ^ " of " ^ print_paramList b ^ print_newType nxt
      | _ -> failwith("Type declaration error 2")
      )
   in 
    let rec print_types_inner t2 = (
      match t2 with 
      Declaration(Name a,b,c) -> a ^ " = " ^ print_newType b ^ if c == EndDecl then print_types_inner c else "\n\n and " ^ print_types_inner c
      | EndDecl -> ";;\n"
      | _ -> failwith("Type declaration error 3")
      ) 
   in
       "type " ^ print_types_inner t
  ;;

(* Permette di ottenere da una lista di parametri l'ultimo elemento e una nuova lista senza quest'ultimo *)
let rec getLastParam pl = match pl with 
  | ParamList(p,NoneP) -> (p,NoneP)
  | ParamList(p,nxt) -> let (p1,nxt1) = getLastParam nxt in (p1, ParamList(p,nxt1))
  | NoneP -> (Name "unit",NoneP);;

(* Aggiorna un predicato già valutato *)
let updateEvalPredicate v n = match v with
  Predicate{rows = a} -> Predicate{rows = n::a}
  | _ -> failwith("Error");;

(* Aggiorna una riga di funzione già valutata *)
let updateEvalFunction v n = match v with
  Function{rows = a} -> Function{rows = n::a}
  | _ -> failwith("Error");;
  
(* Stampa un nuovo contesto *)
let printContext v2 =
    let printParam v = match v with 
    Name v1 -> v1
    | Variable v1 ->  v1 
    | TypeS v1 ->  v1
    | _ -> failwith("Error")
  in  match v2 with
  ParamList(v,nxt) -> (
    match nxt with 
      ParamList(v1,nxt1) -> (
        match nxt1 with 
          ParamList(v2,_) -> let (count,_) = (!nextContext,incr nextContext) in  
            "module E"^ (count |> string_of_int) ^ " = Map.Make("^printParam v1 ^");;\n" ^
            "type " ^ printParam v ^ " = " ^ printParam v2 ^ " E" ^ (count |> string_of_int) ^".t;;\n" ^
            "let member"^ printParam v ^ " _C x = E"^ (count |> string_of_int) ^".find x _C;;\n" ^
            "let add"^ printParam v ^ " _C x t1 = E"^ (count |> string_of_int) ^".add x t1 _C;;\n"
          | NoneP -> failwith("Error")
      )
      | NoneP -> failwith("Error")
  )
  | NoneP -> failwith("Error")
;;

  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)     
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)

  let print_term t = 
    let rec print_paramList pm = 
        begin
          match pm with
          NoneP -> " 'a "
          | ParamList(Name vl,nxt) -> ( if  (compare vl "term") == 0 || Hashtbl.mem contains_term vl   then " 'a " else "" ) ^ vl ^ " * " ^ print_paramList nxt
          | _ -> failwith("Type declaration error 1")
        end
   in
    let rec print_newType nt = 
        begin
          match nt with 
          NoneP -> ""
          | ParamList(TypeS a,nxt) -> "\n\t\t | " ^ a ^ " of 'a " ^ print_newType nxt
          | ParamList(Type(a,b),nxt) -> "\n\t\t | " ^ a ^ " of " ^ print_paramList b ^ print_newType nxt
          | _ -> failwith("Type declaration error 2")
        end
   in 
    let print_types_inner t2 = 
      begin
        match t2 with 
        Declaration(Name a,b,_) -> a ^ " = " ^ print_newType b
        | EndDecl -> ";;\n"
        | _ -> failwith("Type declaration error 3")
      end
   in
       " 'a " ^ print_types_inner t
  ;;


  let print_types_incremental t = 
    let rec check_paramList pm = (
        match pm with
        NoneP -> false
        | ParamList(Name vl,nxt) -> 
        ( 
          if (compare vl "term") == 0  then
            true 
           else false
        ) || check_paramList nxt
        | _ -> failwith("Type declaration error 1")
        )
   in
    let rec check_term nt = (
      match nt with 
      NoneP -> false
      | ParamList(TypeS _,nxt) -> false ||  check_term nxt
      | ParamList(Type(_,b),nxt) -> check_paramList b || check_term nxt
      | _ -> failwith("Type declaration error 2")
      )
   in  
    let rec print_paramList pm recursive = (
        match pm with
        NoneP -> ""
        | ParamList(Name vl,nxt) -> 
        ( 
          if (compare vl "term") == 0 || Hashtbl.mem contains_term vl || recursive  then
           " 'a " 
           else "" 
        ) ^ vl ^ if nxt == NoneP then print_paramList nxt recursive else " * " ^ print_paramList nxt recursive
        | _ -> failwith("Type declaration error 1")
        )
   in
    let rec print_newType nt recursive = (
      match nt with 
      NoneP -> ""
      | ParamList(TypeS a,nxt) -> "\n\t\t | " ^ a ^ print_newType nxt recursive
      | ParamList(Type(a,b),nxt) -> "\n\t\t | " ^ a ^ " of " ^ print_paramList b recursive ^ print_newType nxt recursive
      | _ -> failwith("Type declaration error 2")
      )
   in 
    let rec print_types_inner t2 = (
      match t2 with 
      Declaration(Name a,b,c) -> (
        if (compare a "term") == 0 then 
          print_term t2 
        else 
        let recursive = check_term b in (
             if recursive then 
               let _ = Hashtbl.add contains_term a true in " 'a "
            else 
               ""
          ) ^ a ^ " = " ^ print_newType b recursive ) ^ if c == EndDecl then print_types_inner c else "\n\n\t and " ^ print_types_inner c
      | EndDecl -> ";;\n"
      | _ -> failwith("Type declaration error 3")
      ) 
   in
       "\ttype " ^ print_types_inner t
  ;;

  let rec loop_rules_incremental r = match r with 
  None -> Eval.empty
  | Formula(line,a,b) ->  let m = loop_rules_incremental b in (
      match a with
        Atom(c,d) -> 
          let (lst, pl) = getLastParam d in 
            if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
        | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
        | Context(v2) -> let _ = printContext v2 |> print_endline in m
        | _ -> failwith("Error in atom def")
      )
  | Rule(line,a,b,c) -> let m = loop_rules_incremental c in (
    match a with
      Atom(d,e) -> 
      let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
      | _ -> m
    )
  | OcamlEmbedded(s,n) -> let _ = s |> print_endline in loop_rules_incremental n
  | DeclarationType(dec,n) -> let _ = dec |> print_types_incremental |> print_endline in loop_rules_incremental n;;
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)     
  (***********************************************************************************************************)
  (***********************************************************************************************************)
  (***********************************************************************************************************)

(* Prende in input l'AST e restuisce una mappa che associata al nome della regola una lista di coppie tuple => Lista di atomici (tuple sono i valori matchati in Ocaml e la lista di atomici le operazioni da fare) *)
let rec loop_rules r = match r with 
  None -> Eval.empty
  | Formula(line,a,b) ->  let m = loop_rules b in (
      match a with
        Atom(c,d) -> 
          let (lst, pl) = getLastParam d in 
            if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
        | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
        | Context(v2) -> let _ = printContext v2 |> print_endline in m
        | _ -> failwith("Error in atom def")
      )
  | Rule(line,a,b,c) -> let m = loop_rules c in (
    match a with
      Atom(d,e) -> 
      let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
      | _ -> m
    )
  | OcamlEmbedded(s,n) -> let _ = s |> print_endline in loop_rules n
  | DeclarationType(dec,n) -> let _ = dec |> print_types |> print_endline in loop_rules n;;

  (* Stampa una tupla con i valori contenuti *)
  let rec printTuple t = 
        let printParam v = match v with 
          Name v1 ->  v1
          | Variable v1 ->  "_" ^ v1 
          | TypeS v1 -> v1
          | Type(v1,v2) -> v1 ^ printTuple v2
      in
        let rec printInnerTuple t2 = (match t2 with
          ParamList(v1,NoneP) -> printParam v1 ^ printInnerTuple NoneP
          | ParamList(v1,v2) -> printParam v1 ^ " , " ^ printInnerTuple v2
          | NoneP -> ")")
      in 
        "(" ^ printInnerTuple t
    ;;  
  
    (* Stampa una tupla con i valori contenuti *)
  let printParams t = 
    let printParam v = match v with 
      Name v1 ->  v1
      | Variable v1 ->  "_" ^ v1 
      | TypeS v1 -> v1
      | Type(v1,v2) -> "(" ^ v1 ^ printTuple v2 ^ ")"
  in
    let rec printInnerTuple t2 = (match t2 with
      ParamList(v1,NoneP) -> printParam v1 ^ printInnerTuple NoneP
      | ParamList(v1,v2) -> printParam v1 ^ " " ^ printInnerTuple v2
      | NoneP -> " ")
  in 
    " " ^ printInnerTuple t
;;  

  (* Stampa un parametro *)
  let printParam v = match v with 
    Name v1 -> v1
    | Variable v1 -> "_" ^ v1 
    | TypeS v1 ->  v1
    | Type(v1,v2) -> v1 ^ printTuple v2
  ;;  
  (* Stampa la riga di un predicato *)
  let rec printResult r = match r with
    AtomList(v1,None) -> (
        match v1 with 
          Atom(v3,v4) -> v3 ^ printParams v4 
          | Keyword(v3,v4) -> v3 ^ printParams v4
          | _ -> failwith("Error")
      ) 
    | AtomList(v1,v2) -> (
        match v1 with 
          Atom(v3,v4) -> v3 ^ printParams v4 ^ " && " ^ printResult v2
          | Keyword(v3,v4) -> v3 ^ printParams v4 ^ " && " ^ printResult v2
          | _ -> failwith("Error")
      ) 
    | None -> "true";;

  (* Stampa la riga di una effettiva funzione *)
  let rec printResultState al final = 
    let printAdd pl f2 nxt = 
      (match pl with 
        ParamList(v1,nxt1) -> (
          match nxt1 with 
            ParamList(v2,nxt2) -> (
              match nxt2 with 
                ParamList(v3,nxt3) -> (
                  match nxt3 with
                    ParamList(v4,nxt4) -> (
                      match nxt4 with
                        ParamList(v5,_) -> (
                          match v5 with 
                            Name nm -> " let " ^ nm ^ " = add" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ " " ^ printParam v4 ^ " in " ^ printResultState nxt f2
                            | Variable nm -> " let " ^ "_" ^ nm ^ " = add" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ " " ^ printParam v4 ^ " in " ^ printResultState nxt f2
                            | TypeS nm ->  " let " ^ nm ^ " = add" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ " " ^ printParam v4 ^ " in " ^ printResultState nxt f2
                            | Type(nm,nm1) -> "( match add" ^ printParam v1 ^ " "^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ " " ^ printParam v4 ^ " with " ^
                                 nm ^ " " ^ printTuple nm1 ^ " -> " ^ printResultState nxt f2 ^ " | _ -> failwith(\"Error!\"))"
                        )
                        | _ -> failwith("Error Add Def")
                    )
                    | _ -> failwith("Error Add Def")
                )
                | _ -> failwith("Error Add Def")
            )
            | _ -> failwith("Error Add Def")
        )
        | _ -> failwith("Error Add Def"))
        in
        let printMember pl f2 nxt = 
          (match pl with 
            ParamList(v1,nxt1) -> (
              match nxt1 with 
                ParamList(v2,nxt2) -> (
                  match nxt2 with 
                    ParamList(v3,nxt3) -> (
                      match nxt3 with
                            ParamList(v5,_) -> (
                              match v5 with 
                                Name nm -> " let " ^ nm ^ " = member" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ " in " ^ printResultState nxt f2
                                | Variable nm -> " let " ^ "_" ^ nm ^ " = member" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ "  in " ^ printResultState nxt f2
                                | TypeS nm ->  " let " ^ nm ^ " = member" ^ printParam v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ "  in " ^ printResultState nxt f2
                                | Type(nm,nm1) -> "( match member" ^ printParam v1 ^ " "^ " " ^ printParam v2 ^ " " ^ printParam v3  ^ "with " ^
                                     nm ^ " " ^ printTuple nm1 ^ " -> " ^ printResultState nxt f2 ^ " | _ -> failwith(\"Error!\"))"
                            )
                            | _ -> failwith("Error Add Def")
                        )
                    | _ -> failwith("Error Add Def")
                )
                | _ -> failwith("Error Add Def")
            )
            | _ -> failwith("Error Add Def"))
            in
   (
     match al with
       AtomList(a,nxt) -> (
        match a with 
        Atom(name, pl) -> 
          let (lst, pl1) = getLastParam pl in 
            (
              match lst with 
                Name v1 -> " let " ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
                | Variable v1 -> " let " ^ "_" ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
                | TypeS v1 ->  " let " ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
                | Type(v1,v2) -> "( match " ^ name ^ " " ^ printParams pl1 ^ " with " ^ v1 ^ " " ^ printTuple v2 ^ " -> " ^ printResultState nxt final ^ " | _ -> failwith(\"Error!\"))"
            )
        | Keyword(name,pl) -> "if " ^ name ^ " " ^ printParams pl ^ " then " ^ printResultState nxt final ^ " else failwith(\"Error!\")"
        | Add(pl) -> printAdd pl final nxt
        | Member(pl) -> printMember pl final nxt
        | _ -> failwith("Error")
      )
      | None -> printParam final
   );; 

  (* Stampa una nuova riga di un predicato *)
  let printNewRowPredicate a b = match a with 
    (c,d,line) ->  "\n (* Line "^ (line |> string_of_int) ^" *) \n | " ^ printTuple c ^ " -> " ^ printResult d ^ b;;

  (* Stampa una nuova riga di funzione *)
  let printNewRowFunction a b = match a with 
   (c,d,e,line) ->  "\n (* Line "^ (line |> string_of_int) ^" *) \n |  " ^ printTuple c ^ " -> " ^ printResultState d e ^ b;;

  (* Stampa un'intestazione di funzione a partire da una lista di coppie a tre argomenti *)
  let printListInput3args ls = 
    let rec countParam p num = (
      match p with 
        ParamList(_,nxt) -> "_e"^ ( num |> string_of_int) ^ " " ^ countParam nxt (num + 1)
        | NoneP -> "" 
    ) in match ls with 
    [] -> " "
    | (c,_,_)::_ -> countParam c 0
  ;;

  (* Stampa un'intestazione di funzione a partire da una lista di coppie a quattro argomenti *)
  let printListInput4args ls = 
    let rec countParam p num = (
      match p with 
        ParamList(_,nxt) -> "_e"^ ( num |> string_of_int) ^ " " ^ countParam nxt (num + 1)
        | NoneP -> "" 
    ) in match ls with 
    [] -> " "
    | (c,_,_,_)::_ -> countParam c 0
  ;;

  (* Stampa una tupla su cui fare il match a partire da una lista di coppie a tre argomenti *)
  let printListInputMatch3args ls = 
    let rec countParam p num = (
      match p with 
        ParamList(_,nxt) -> "_e"^ ( num |> string_of_int) ^ if nxt == NoneP then countParam nxt (num + 1) else  "," ^ countParam nxt (num + 1)
        | NoneP -> "" 
    ) in match ls with 
    [] -> " "
    | (c,_,_)::_ -> countParam c 0
  ;;

  (* Stampa una tupla su cui fare il match a partire da una lista di coppie a quattro argomenti *)
  let printListInputMatch4args ls = 
    let rec countParam p num = (
      match p with 
        ParamList(_,nxt) -> "_e"^ ( num |> string_of_int) ^ if nxt == NoneP then countParam nxt (num + 1) else  "," ^ countParam nxt (num + 1)
        | NoneP -> "" 
    ) in match ls with 
    [] -> " "
    | (c,_,_,_)::_ -> countParam c 0
  ;;

  (** Viene stampata la nuova regola differenziandole tra predicato e funzione *)
  let printRules k v = match v with 
    Predicate{rows = a} -> ("\nlet [@warning \"-27\"] rec " ^ k ^ " "^ printListInput3args a ^" = match ("^ printListInputMatch3args a ^") with " ^ (List.fold_right printNewRowPredicate a "\n | _ -> false;;")) |> print_endline
    | Function{rows = a} -> ("\nlet [@warning \"-27\"] rec " ^ k ^ " "^ printListInput4args a ^" = match ("^ printListInputMatch4args a ^") with " ^ (List.fold_right printNewRowFunction a "\n | _ -> failwith(\"Error!\");;")) |> print_endline;;
  
  (* Itera tutte le regole valutate in modo da stamparle *)
  let print_rules r = Eval.iter printRules r;;





