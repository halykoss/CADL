(** Parametro usato dentro le() delle regole e delle Formule *)
type param = Name of string | Variable of string | Type of string * paramList | TypeS of string | List of string | Every | Num of int
(** Lista di parametri *)
and paramList = ParamList of param * paramList | NoneP;;
(** Dichiarazione di nuovi tipi *)
type declarationType = Declaration of param * paramList * declarationType | EndDecl;;
(** Fomrula atomica *)
type atom = Atom of string * paramList | ForContext of string * paramList | Keyword of string * paramList | Context of paramList | Add of paramList | Member of paramList | Map of paramList;;
(** Lista di formule atomiche *)
type atomList = AtomList of atom * atomList | None;;

let contains_term = Hashtbl.create 123456;;

let nextContext = ref 0;;
let isCompatEnvDec = ref false;;
let compatEnv = ref "";;
let decTerm = ref (Declaration(Name "unit",NoneP,EndDecl));;
(* Lista di input (Regole, Formule, Codice Ocaml Embedded, nuovi tipi ) *)
type input = 
    Rule of int * atom * atomList * input 
  | Formula of int * atom * input 
  | OcamlEmbedded of string * input 
  | OceNotInc of string * input
  | OnlyInc of string * input
  | DeclarationType of declarationType * input 
  | CompatEnv of string * input
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
    | ParamList(List vl,nxt) -> vl ^ " list" ^ if nxt == NoneP then print_paramList nxt else " * " ^ print_paramList nxt
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
  | _ -> failwith("Error 3");;

(* Aggiorna una riga di funzione già valutata *)
let updateEvalFunction v n = match v with
    Function{rows = a} -> Function{rows = n::a}
  | _ -> failwith("Error 4");;

(* Stampa un nuovo contesto *)
let printContext v2 =
  let printParam v = match v with 
      Name v1 -> v1
    | Variable v1 ->  v1 
    | TypeS v1 ->  v1
    | List v1 -> v1 ^ " list"
    | _ -> failwith("Error 5")
  in  match v2 with
    ParamList(v,nxt) -> (
      match nxt with 
        ParamList(v1,nxt1) -> (
          match nxt1 with 
            ParamList(v2,nxt2) -> (
              match nxt2 with
                ParamList(v3,_) -> (

                  "type " ^ printParam v ^ " = " ^ printParam v2 ^ " "^ printParam v3 ^".t;;\n" ^
                  "let member"^ printParam v ^ " _C x = "^ printParam v3 ^".find _C x;;\n" ^
                  "let add"^ printParam v ^ " _C x t1 = "^ printParam v3 ^".add x t1 _C;;\n"
                )
              | NoneP -> (
                  let (count,_) = (!nextContext,incr nextContext) in  
                  "module E"^ (count |> string_of_int) ^ " = Hashtbl.Make("^printParam v1 ^");;\n" ^
                  "type " ^ printParam v ^ " = " ^ printParam v2 ^ " E" ^ (count |> string_of_int) ^".t;;\n" ^
                  "let member"^ printParam v ^ " _C x = E"^ (count |> string_of_int) ^".find _C x ;;\n" ^
                  "let add"^ printParam v ^ " _C x t1 = E"^ (count |> string_of_int) ^".add x t1 _C;;\n"
                )
            )
          | NoneP -> failwith("Error 6")
        )
      | NoneP -> failwith("Error 7")
    )
  | NoneP -> failwith("Error 8")
;;

(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)     
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
let print_term_getannot t = 
  let printTupleAnnot t1 =
    let rec printTuple t1 = (match t1 with
          ParamList(_,nxt) -> "_," ^ printTuple nxt
        | NoneP -> "annot)")
    in "(" ^ printTuple t1
  in 
  let printParam v = (match v with 
        Name v1 ->  v1 ^ " annot"
      | TypeS v1 -> v1 ^ " annot"
      | List v1 -> v1 ^ " list annot"
      | Type(v1,v2) -> v1 ^ printTupleAnnot v2
      | _ -> failwith("Error 9"))
  in 
  let rec printRowAnnot t1 = (
    match t1 with 
    | ParamList(p,nxt) -> "\n\t\t\t| " ^ printParam p ^ printRowAnnot  nxt
    | NoneP -> " -> annot;;"
  ) in
  match t with 
    Declaration(_,b,_) -> 
    "\tlet term_getannot t = \n\t\tmatch t with" ^ printRowAnnot b
  | _ -> failwith("Type declaration error 3")
;;

let print_term_edit t = 
  let printTupleEdit t1 =
    let rec printTuple t1 idx = (match t1 with
          ParamList(_ ,nxt) -> "e" ^ (idx |> string_of_int) ^ "," ^ printTuple nxt (idx + 1)
        | NoneP -> "_)")
    in "(" ^ printTuple t1 0
  in 
  let printTupleFinal t1 =
    let rec printTuple t1 idx = (match t1 with
          ParamList(Name v ,nxt) -> "e" ^ (idx |> string_of_int) ^(if (compare v "term") == 0 then "'" else "") ^ "," ^ printTuple nxt (idx + 1)
        |  ParamList(List v ,nxt) -> 
          (if (compare v "term") == 0 then "ti" else ("e" ^ (idx |> string_of_int))) ^ "," ^ printTuple nxt (idx + 1)
        | NoneP -> "a)"
        | _ -> failwith("Error 1"))
    in "(" ^ printTuple t1 0
  in 
  let printParam v = (
    match v with 
      Name v1 ->  v1 ^ " _"
    | TypeS v1 -> v1 ^ " _"
    | Type(v1,v2) -> v1 ^ printTupleEdit v2
    | _ -> failwith("Error 2"))
  in     
  let printParamFinal v = (
    match v with 
      Name v1 ->  v1 ^ " a"
    | TypeS v1 -> v1 ^ " a"
    | Type(v1,v2) -> v1 ^ printTupleFinal v2
    | _ -> failwith("Error 3"))
  in       
  let printList v = 
    let rec printListEdit v1 idx = (
      match v1 with
        ParamList(Name v1, nxt) -> (if (compare v1 "term") == 0 then "e" ^(idx |> string_of_int) ^ "';" else "" ) ^  printListEdit nxt (idx + 1)
      | NoneP -> ""
      | ParamList(_, nxt) -> "" ^ printListEdit nxt (idx + 1)
    ) in (
      match v with 
      | Type(_,v2) -> printListEdit v2 0
      | _ -> "")
  in 
  let contains_term_list ls = (
    let rec check_term_list ls = 
        match ls with
        ParamList(List _, _) -> true
        | ParamList(_,nxt) -> check_term_list nxt
        | NoneP -> false
      in
    match ls with
      Type(_,v2) -> check_term_list v2
      | _ -> false
  ) in
  let rec printRowAnnot t1 = (
    match t1 with 
    | ParamList(p,nxt) -> "\n\t\t\t| (" ^ printParam p ^", "^ (if contains_term_list p then "_"  else "["^ printList p ^"]") ^") -> " ^ printParamFinal p ^ printRowAnnot  nxt
    | NoneP -> "\n\t\t\t| _ -> failwith(\"Error\");;"
  ) in
  match t with 
    Declaration(_,b,_) -> 
    "\n\tlet term_edit (t : 'a term) (ti : ('b term) list) (a : 'b) : ('b term) =\n\t\tmatch (t, ti) with" ^ printRowAnnot b
  | _ -> failwith("Type declaration error 3")
;;

let print_get_sorted_children t = 
  let printTupleEdit t1 =
    let rec printTuple t1 idx = (match t1 with
        | ParamList(Name v,nxt) -> (if (compare v "term") == 0 then "e" ^ (idx |> string_of_int) else "_") ^ "," ^ printTuple nxt (idx + 1)
        | ParamList(List v,nxt) -> (if (compare v "term") == 0 then "e" ^ (idx |> string_of_int) else "_") ^ "," ^ printTuple nxt (idx + 1)
        |  ParamList(_ ,nxt) ->"_," ^ printTuple nxt (idx + 1)
        | NoneP -> "_)")
    in "(" ^ printTuple t1 0
  in
  let printParam v = (
    match v with 
      Name v1 ->  v1 ^ " _"
    | TypeS v1 -> v1 ^ " _"
    | Type(v1,v2) -> v1 ^ printTupleEdit v2
    | _ -> failwith("Error 10"))
  in      
  let printList v = 
    let rec printListEdit v1 idx count = (
      match v1 with
        ParamList(Name v1, nxt) -> 
          (if (compare v1 "term") == 0 then "(" ^(count |> string_of_int) ^ ",e"^ (idx |> string_of_int) ^");" else "" ) ^  
          (printListEdit nxt (idx + 1) (if (compare v1 "term") == 0 then count + 1 else count ))
      | 
        ParamList(List v1, nxt) -> 
          (if (compare v1 "term") == 0 then "] @ ( (List.combine (enumerate 0 (List.length e"^ (idx |> string_of_int) ^")) e"^ (idx |> string_of_int) ^")) @ [" else "" ) ^  
          (printListEdit nxt (idx + 1) (if (compare v1 "term") == 0 then count + 1 else count ))
      | NoneP -> ""
      | ParamList(_, nxt) -> "" ^ printListEdit nxt (idx + 1) count
    ) in (
      match v with 
      | Type(_,v2) -> printListEdit v2 0 0
      | _ -> "")
  in 
  let rec printRowChildren t1 = (
    match t1 with 
    | ParamList(p,nxt) -> "\n\t\t\t| " ^ printParam p ^" -> [" ^ printList p ^ "]" ^ printRowChildren  nxt
    | NoneP -> "\n\t\t\t;;"
  ) in
  match t with 
    Declaration(_,b,_) -> 
    "\n\tlet get_sorted_children (t : 'a term) : ((int * 'a term) list) =\n\t\tmatch t with" ^ printRowChildren b
  | _ -> failwith("Type declaration error 3")
;;

let print_term t = 
  let rec print_paramList pm = 
    begin
      match pm with
        NoneP -> " 'a "
      | ParamList(Name vl,nxt) -> ( if  (compare vl "term") == 0 || Hashtbl.mem contains_term vl   then " 'a " else "" ) ^ vl ^ " * " ^ print_paramList nxt
      | ParamList(List vl,nxt) -> ( if  (compare vl "term") == 0 || Hashtbl.mem contains_term vl   then " 'a " else "" ) ^ vl ^ " list * " ^ print_paramList nxt
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
  decTerm := t;" 'a " ^ print_types_inner t
;;


let print_types_incremental t = 
  let rec check_paramList pm = (
    match pm with
      NoneP -> false
    | ParamList(List vl,nxt)
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
    | ParamList(List vl,nxt) -> 
      ( 
        if (compare vl "term") == 0 || Hashtbl.mem contains_term vl || recursive  then
          " 'a " 
        else "" 
      ) ^ vl ^ " list" ^ if nxt == NoneP then print_paramList nxt recursive else " * " ^ print_paramList nxt recursive
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
  | Formula(_,Context(v2),b) -> let _ = printContext v2 |> print_endline in loop_rules_incremental b
  | Formula(line,a,b) ->  let m = loop_rules_incremental b in (
      match a with
        ForContext(c,d) 
        | Atom(c,d) -> 
        let (lst, pl) = getLastParam d in 
        if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
      | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
      | Context(v2) -> let _ = printContext v2 |> print_endline in m
      | _ -> failwith("Error in atom def")
    )
  | Rule(line,a,b,c) -> let m = loop_rules_incremental c in (
      match a with
      ForContext(d,e) 
      | Atom(d,e) -> 
        let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
      | _ -> m
    )
  | OcamlEmbedded(s,n) -> let _ = s |> print_endline in let m = loop_rules_incremental n in m
  | DeclarationType(dec,n) -> let _ = dec |> print_types_incremental |> print_endline in loop_rules_incremental n
  | OceNotInc(_,n) -> loop_rules_incremental n
  | OnlyInc(s,n) -> let _ = s |> print_endline in loop_rules_incremental n
  | CompatEnv(s,nxt) -> let _ = compatEnv := s;isCompatEnvDec := true in loop_rules_incremental nxt;;

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
    | Formula(_,Context(v2),b) -> let _ = printContext v2 |> print_endline in loop_rules b
  | Formula(line,a,b) ->  let m = loop_rules b in (
      match a with
      ForContext(c,d) 
      | Atom(c,d) -> 
        let (lst, pl) = getLastParam d in 
        if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
      | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
      | Context(v2) -> let _ = printContext v2 |> print_endline in m
      | _ -> failwith("Error in atom def")
    )
  | Rule(line,a,b,c) -> let m = loop_rules c in (
      match a with
      ForContext(d,e) 
      | Atom(d,e) -> 
        let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
      | _ -> m
    )
  | OcamlEmbedded(s,n) -> let _ = s |> print_endline in loop_rules n
  | OceNotInc(s,n) -> let _ = s |> print_endline in loop_rules n
  | DeclarationType(dec,n) -> let _ = dec |> print_types |> print_endline in loop_rules n
  | OnlyInc(_,nxt)
  | CompatEnv (_,nxt) -> loop_rules nxt;;

  let rec loop_rules_normal r = match r with 
    None -> Eval.empty
  | Formula(line,a,b) ->  let m = loop_rules_normal b in (
      match a with
      ForContext(c,d) 
      | Atom(c,d) -> 
        let (lst, pl) = getLastParam d in 
        if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
      | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
      | Context(_) -> m
      | _ -> failwith("Error in atom def")
    )
  | Rule(line,a,b,c) -> let m = loop_rules_normal c in (
      match a with
      ForContext(d,e) 
      | Atom(d,e) -> 
        let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
      | _ -> m
    )
  | OcamlEmbedded(_,nxt)
  | DeclarationType(_,nxt)
  | OceNotInc(_,nxt)
  | OnlyInc(_,nxt)
  | CompatEnv (_,nxt) -> loop_rules_normal nxt;;

(* Stampa una tupla con i valori contenuti *)
let rec printTuple t = 
  let printParam v = match v with 
      Name v1 ->  v1
    | Variable v1 ->  "_" ^ v1 
    | TypeS v1 -> v1
    | Type(v1,v2) -> v1 ^ printTuple v2
    | List v1 -> v1 ^ " list"
    | Every -> "_"
    | Num v1 -> v1 |> string_of_int
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
    | List v1 -> v1 ^ " list"
    | Every -> "_"
    | Num v1 -> v1 |> string_of_int
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
  | List v1 -> v1 ^ " list"
  | Every -> "_"
  | Num v1 -> v1 |> string_of_int
;;  
(* Stampa la riga di un predicato *)
let rec printResult r = match r with
    AtomList(v1,None) -> (
      match v1 with 
        Atom(v3,v4) -> v3 ^ printParams v4 
      | Keyword(v3,v4) -> v3 ^ printParams v4
      | _ -> failwith("Error 11")
    ) 
  | AtomList(v1,v2) -> (
      match v1 with 
        Atom(v3,v4) -> v3 ^ printParams v4 ^ " && " ^ printResult v2
      | Keyword(v3,v4) -> v3 ^ printParams v4 ^ " && " ^ printResult v2
      | _ -> failwith("Error 12")
    ) 
  | None -> "true";;

(* Stampa la riga di una effettiva funzione *)
let rec printResultState al final = 
  (
    match al with
      AtomList(a,nxt) -> (
        match a with 
          ForContext(name,pl)
          | Atom(name, pl) -> 
          let (lst, pl1) = getLastParam pl in 
          (
            match lst with 
              Name v1 -> " let " ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
            | Variable v1 -> " let " ^ "_" ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
            | TypeS v1 ->  " let " ^ v1 ^ " = " ^ name ^ " " ^ printParams pl1 ^ " in " ^ printResultState nxt final
            | Type(v1,v2) -> "( match " ^ name ^ " " ^ printParams pl1 ^ " with " ^ v1 ^ " " ^ printTuple v2 ^ " -> " ^ printResultState nxt final ^ " | _ -> failwith(\"Error!\"))"
            | _ -> failwith("Erro")
          )
        | Keyword(name,pl) -> "if " ^ name ^ " " ^ printParams pl ^ " then " ^ printResultState nxt final ^ " else failwith(\"Error!\")"
        | Add(pl) -> printAdd pl final nxt
        | Member(pl) -> printMember pl final nxt
        | Map(pl) -> let (lst, pl1) = getLastParam pl in 
          "let " ^ (lst |> printParam) ^ " = listMap " ^ ( pl1|> printParams) ^ " in " ^  printResultState nxt final
        | _ -> failwith("Error 13")
      )
    | None -> printParam final
  )
and printAdd pl f2 nxt = 
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
                       | _ -> failwith("Error 15")
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
and  printMember pl f2 nxt = 
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
                   | _ -> failwith("Erro")
                 )
               | _ -> failwith("Error Add Def")
             )
           | _ -> failwith("Error Add Def")
         )
       | _ -> failwith("Error Add Def")
     )
   | _ -> failwith("Error Add Def")) ;; 

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
    Predicate{rows = a} -> ("\nlet [@warning \"-all\"] rec " ^ k ^ " "^ printListInput3args a ^" = match ("^ printListInputMatch3args a ^") with " ^ (List.fold_right printNewRowPredicate a "\n | _ -> false;;")) |> print_endline
  | Function{rows = a} -> ("\nlet [@warning \"-all\"] rec " ^ k ^ " "^ printListInput4args a ^" = match ("^ printListInputMatch4args a ^") with " ^ (List.fold_right printNewRowFunction a "\n | _ -> failwith(\"Error!\");;")) |> print_endline;;

(* Itera tutte le regole valutate in modo da stamparle *)
let print_rules r = Eval.iter printRules r;;


(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)     
(***********************************************************************************************************)
(***********************************************************************************************************)
(***********************************************************************************************************)

let rec printResultStateIncr al final = 
  (
    match al with
      AtomList(a,nxt) -> (
        match a with 
          Atom(name, pl) -> 
          let (lst, pl1) = getLastParam pl in 
          (
            match lst with 
              Name v1
            | Variable v1 
            | TypeS v1 ->  "( match " ^ name ^ " " ^ printParams pl1 ^ " with Some(" ^ v1 ^ ") -> " ^ printResultStateIncr nxt final ^ " | None -> None)"
            | Type(v1,v2) -> "( match " ^ name ^ " " ^ printParams pl1 ^ " with Some(" ^ v1 ^ " " ^ printTuple v2 ^ ") -> " ^ printResultStateIncr nxt final ^ " | None -> None)"
            | _ -> failwith("Erro")
          )
        | Keyword(name,pl) -> "if " ^ name ^ " " ^ printParams pl ^ " then " ^ printResultStateIncr nxt final ^ " else failwith(\"Error!\")"
        | Add(pl) -> printAdd pl final nxt
        | Member(pl) -> printMember pl final nxt
        | _ -> failwith("Error 20")
      )
    | None -> "Some (" ^ printParam final ^ ")"
  );;


(* Stampa una nuova riga di funzione *)
let printNewRowFunctionInc a b = match a with 
    (c,d,e,line) ->  "\n\t\t (* Line "^ (line |> string_of_int) ^" *) \n\t\t |  " ^ printTuple c ^ " -> " ^ printResultStateIncr d e ^ b;;

(* Stampa una tupla con i valori contenuti *)
let printTupleFV t = 
  let printParam v = 
    let rec printTupIn ti = (match ti with
          ParamList(v1,NoneP) -> printParam v1 ^ printTupIn NoneP
        | ParamList(v1,v2) -> printParam v1 ^ " , " ^ printTupIn v2
        | NoneP -> ", a)") in 
    match v with 
      Name v1 ->  v1 
    | Variable v1 ->  "_" ^ v1 
    | TypeS v1 -> v1 ^ "(a)"
    | Type(v1,v2) -> v1 ^ "(" ^ printTupIn v2
    | List v1 -> v1 ^ " list"
    | Every -> "_"
    | Num v1 -> v1 |> string_of_int
  in
  let rec printInnerTupleFV t2 = (match t2 with
        ParamList(v1,NoneP) -> printParam v1 ^ printInnerTupleFV NoneP
      | ParamList(v1,v2) -> printParam v1 ^ " , " ^ printInnerTupleFV v2
      | NoneP -> ")")
  in 
  "(" ^ printInnerTupleFV t
;;  

let getTermInTuple t = match t with
  | ParamList(_,ParamList(v2,_)) -> ParamList(v2,NoneP)
  | _ -> failwith("get Term Error")
;;

let rec printInnerCJ ls idx final = 
  let isSingular v = (
    match v with
      Name _ -> false
    | Variable _ -> false
    | _ -> true
  ) in  
  match ls with 
    AtomList(Atom("type_check",pl),nxt) -> 
    let (lst,_) = getLastParam pl in 
    "(match List.nth_opt rs " ^ ( idx |> string_of_int) ^ " with Some(" ^ ( lst|> printParam) ^ ") -> ("^ (printInnerCJ nxt (idx + 1) final ^")" ^ if isSingular lst then (" \t| Some _ -> None \t| None -> None") else "\t| None -> None") ^ ")"
  | AtomList(Atom(s,pl),nxt) -> 
    let (lst,ls) = getLastParam pl in 
    "(match " ^ s ^ " " ^ (ls |> printParams) ^ " with " ^ (lst |> printParam) ^ " -> " ^ printInnerCJ nxt idx final ^ (if isSingular lst then " \t | _ -> None" else "") ^ ")"
  | AtomList(Map(pl),nxt) -> 
    let (lst,_) = getLastParam pl in 
    "(match rs with " ^ (lst |> printParam) ^ " -> " ^ printInnerCJ nxt idx final ^ (if isSingular lst then  "  | _ -> None)" else ")")
  |AtomList(Keyword("compat",pl),nxt) ->
    (*"(match check " ^ (pl |> printParams) ^ " with " ^  "Some _ -> " ^ printInnerCJ nxt idx final ^ "| None -> None)"*)
    "(if check " ^ (pl |> printParams) ^ " then " ^  " ( " ^ printInnerCJ nxt idx final ^ " ) else None)"
  | AtomList(Keyword(s,pl),nxt) ->
    "(if " ^ s ^ " " ^ (pl |> printParams) ^ " then " ^  " ( " ^ printInnerCJ nxt idx final ^ " ) else None)"
  | None -> " Some ("^ (final |> printParam) ^ ")"
  | AtomList(Member(ParamList(p1,ParamList(p2,ParamList(p3,ParamList(p4,NoneP))))),nxt) ->
    "(match member"^ (p1 |> printParam) ^" " ^ (p2 |> printParam) ^ " " ^ (p3 |> printParam) ^" with " ^ (p4 |> printParam) ^ " -> "^ (printInnerCJ nxt idx final) ^ (if isSingular p4 then  "  | _ -> None)" else ")")
  | AtomList(_,nxt) -> printInnerCJ nxt idx final
;;

let printNewRowFunctionTR a b = match a with 
    (c,d,e,line) ->  
    "\n\t\t (* Line "^ (line |> string_of_int) ^" *) \n\t\t |  " 
    ^ ( c |> getTermInTuple |> printTupleFV) ^ " ->  " ^ printInnerCJ d 0 e ^ " " ^ b;;

let rec changeAtomList al = match al with
  | AtomList(Keyword("compat",pl),nxt) -> AtomList(Keyword("check",pl),changeAtomList nxt)
  |  AtomList(a,nxt) -> AtomList(a, changeAtomList nxt)
  | None -> None;;

let rec modifyRows a = match a with
    (c,d,line)::xs -> (c,changeAtomList d,line)::(modifyRows xs)
  | [] -> []
;;
(** Viene stampata la nuova regola differenziandole tra predicato e funzione *)
let printRulesCheck k v = match v with 
  | Predicate{rows = a} -> ("\t\tlet [@warning \"-all\"] rec " ^ k ^ " "^ printListInput3args a ^" = (match ("^ printListInputMatch3args a ^") with " ^ (List.fold_right printNewRowPredicate (modifyRows a) "\n\t\t | _ -> false)")) |> print_endline
  | Function{rows = a} -> ("\tlet [@warning \"-all\"] rec " ^ k ^ " "^ printListInput4args a ^" = match ("^ printListInputMatch4args a ^") with " ^ (List.fold_right printNewRowFunctionInc a "\n\t\t | _ -> None")) |> print_endline;;


let printCheck_join t = 
  let _ = ("let checkjoin (t : (int * VarSet.t) term) (_C : context) (rs : res list) : res option =" |> print_endline) in
  let value = Eval.find "compat" t in 
  (*let value = Eval.find "check" t in *)
  let map = Eval.add "check" value (Eval.empty) in 
  let _ = Eval.iter printRulesCheck map in
  let _ = "in match t with" |> print_endline in 
  let value = Eval.find "type_check" t in
  match value with 
    Function{rows=a} -> (List.fold_right (printNewRowFunctionTR) a "\n\t\t | _ -> failwith(\"Term not is not evaluable\");;\n\n") |> print_endline
  | _ -> failwith "Error 1"
;;

(* Stampa un parametro *)
let printParam v = match v with 
    Name v1 -> v1
  | Variable v1 -> "_" ^ v1 
  | TypeS v1 ->  v1
  | Type(v1,v2) -> v1 ^ printTuple v2
  | List v1 -> v1 ^ " list"
  | Every -> "_"
  | Num v1 -> v1 |> string_of_int
;;  

(* Stampa una nuova riga di funzione *)
let printNewRowFunctionFV a b = match a with 
    (c,d,e,line) ->  "\n\t\t (* Line "^ (line |> string_of_int) ^" *) \n\t\t |  " ^ printTupleFV c ^ " -> " ^ printResultState d e ^ b;;

(** Viene stampata la nuova regola differenziandole tra predicato e funzione *)
let printRulesFV k v = match v with 
  | Function{rows = a} -> ("\t\tlet [@warning \"-all\"] rec " ^ k ^ " "^ printListInput4args a ^" = (match ("^ printListInputMatch4args a ^") with " ^ (List.fold_right printNewRowFunctionFV a "\n\t\t | _ -> _e1 VarSet.empty)")) |> print_endline
  | _ -> failwith("Illegal arg");;

let printFreeVar t = 
  let _ = ("\tlet compute_fv (e: 'a term) : VarSet.t =" |> print_endline) in
  let value = Eval.find "free_variables_cps" t in 
  let map = Eval.add "free_variables_cps" value (Eval.empty) in
  let _ = Eval.iter printRulesFV map in
  ( "\tin \n\tfree_variables_cps e (fun d -> d);;\n" |> print_endline)
;;

let rec containsTypeCheck ls = 
  match ls with 
    AtomList(Atom("type_check",_),_) -> false  
  | None -> true
  | AtomList(_,nxt) -> containsTypeCheck nxt
;;

let rec countTypeCheck ls = 
  match ls with 
    AtomList(Atom("type_check",_),nxt) -> 1 + countTypeCheck nxt
  | None -> 0
  | AtomList(_,nxt) -> countTypeCheck nxt
;;

let rec notContainsContextOp ls = 
  match ls with 
    AtomList(Add(_),_) -> false
  | AtomList(ForContext(_,_),_) -> false
  | None -> true
  | AtomList(_,nxt) -> notContainsContextOp nxt
;;

let isBaseCase ls = match ls with
    AtomList(_,_) -> containsTypeCheck ls
  | None -> true
;;

let rec printStatement t idx remain =
  let isSingular v = (
    match v with
      Name _ -> false
    | Variable _ -> false
    | _ -> true
  ) in  
   match t with
    AtomList(Atom("type_check",ParamList(Variable fst,ls)), nxt) -> 
    let (lst,_) = getLastParam ls in "(match List.nth_opt rs " ^ ( idx |> string_of_int) ^ " with Some(" ^ ( lst|> printParam) ^ ") -> ("^ (printStatement nxt (idx + 1) (remain-1)) ^") \t| None -> _"^ fst ^")"
  | AtomList(Add(ParamList(Name v1,ParamList(v2,ParamList(v3,ParamList(v4,ParamList(v5,NoneP)))))),nxt) -> 
    "let " ^ printParam v5 ^ " = add" ^ v1 ^ " " ^ printParam v2 ^ " " ^ printParam v3 ^ " " ^ printParam v4 ^ " in " ^ printStatement nxt idx remain
  | AtomList(ForContext(s,pl),nxt) -> 
  let (lst,ls) = getLastParam pl in 
  "(match " ^ s ^ " " ^ (ls |> printParams) ^ " with " ^ (lst |> printParam) ^ " -> " ^ printStatement nxt idx remain ^ (if isSingular lst then " \t | _ -> None" else "") ^ ")" 
  | None -> "failwith \"Error\""
  | AtomList(_,nxt) -> printStatement nxt idx remain;;

let printNewRowFunctionTR a b = match a with 
    (c,d,_,line) ->  
    "\n\t\t (* Line "^ (line |> string_of_int) ^" *) \n\t\t |  " 
    ^ ( c |> getTermInTuple |> printTupleFV) ^ " -> " ^ 
    (if isBaseCase d then "failwith(\"Tr invoked on base case\")" else (if notContainsContextOp d then "_C" else printStatement d 0 (countTypeCheck d))) ^ " " ^ b;;

let printTr t =
  let pr = ("\tlet tr (i : int) (ti : (int * VarSet.t) term) (t : (int * VarSet.t) term) (_C : context) (rs : res list) : context =\n\t\tmatch t with" ) in
  let value = Eval.find "type_check" t in
  let _ = pr ^ (
      match value with
        Function{rows=a} -> (List.fold_right printNewRowFunctionTR a "\n\t\t | _ -> failwith(\"Term not is not evaluable\");;")
      | _ -> failwith("Error 2")
    ) |> print_endline in 
  ()
;;

