(** Parametro usato dentro le() delle regole e delle Formule *)
type param = Name of string | Variable of string | Type of string * paramList | TypeS of string
(** Lista di parametri *)
and paramList = ParamList of param * paramList | NoneP;;
(** Dichiarazione di nuovi tipi *)
type declarationType = Declaration of param * paramList * declarationType | EndDecl;;
(** Fomrula atomica *)
type atom = Atom of string * paramList | Keyword of string * paramList;;
(** Lista di formule atomiche *)
type atomList = AtomList of atom * atomList | None;;
(** Lista di input (Regole, Formule, Codice Ocaml Embedded, nuovi tipi ) *)
type input = 
  Rule of int * atom * atomList * input 
  | Formula of int * atom * input 
  | OcamlEmbedded of string * input 
  | DeclarationType of declarationType * input 
  | PrintType of input 
  | None;;

(** Valutazione delle righe contenenti Regole e formule *)
type new_evaluation =
  Predicate of {
    rows     : (paramList * atomList * int) list;
  } 
  | Function of {
    rows     : (paramList * atomList * param * int) list;
  };;

(** Map contenente la lista delle regole e formule valutate *)
module Eval =  Map.Make(String);;

(** Costruisce una stringa con tutti i nuovi tipi definiti dall'utente *)
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
      | EndDecl -> ";;"
      | _ -> failwith("Type declaration error 3")
      ) 
   in
       "type " ^ print_types_inner t
  ;;

(** Permette di ottenere da una lista di parametri l'ultimo elemento e una nuova lista senza quest'ultimo *)
let rec getLastParam pl = match pl with 
  | ParamList(p,NoneP) -> (p,NoneP)
  | ParamList(p,nxt) -> let (p1,nxt1) = getLastParam nxt in (p1, ParamList(p,nxt1))
  | NoneP -> (Name "unit",NoneP);;

(** Aggiorna un predicato già valutato *)
let updateEvalPredicate v n = match v with
  Predicate{rows = a} -> Predicate{rows = n::a}
  | _ -> failwith("Error");;

(** Aggiorna una riga di funzione già valutata *)
let updateEvalFunction v n = match v with
  Function{rows = a} -> Function{rows = n::a}
  | _ -> failwith("Error");;
(** Cerca nell'AST le dichiarazioni di nuovi tipi e li compatta in una lista di Declaration *)
let rec loop_types r = match r with 
  None -> EndDecl
  | Formula(_,_,b) ->  loop_types b
  | Rule(_,_,_,c) -> loop_types c
  | OcamlEmbedded(_,n) -> loop_types n
  | PrintType(nxt) -> loop_types nxt
  | DeclarationType(d,n) -> let next = loop_types n in (
      match d with
      Declaration(a,b,_) -> Declaration(a,b,next)
      | EndDecl -> EndDecl
    );;

(** Prende in input l'AST e restuisce una mappa che associata al nome della regola una lista di coppie tuple => Lista di atomici (tuple sono i valori matchati in Ocaml e la lista di atomici le operazioni da fare) *)
let rec loop_rules newTypes r = match r with 
  None -> Eval.empty
  | Formula(line,a,b) ->  let m = loop_rules newTypes b in (
      match a with
        Atom(c,d) -> 
          let (lst, pl) = getLastParam d in 
            if Eval.mem c m then Eval.add c (updateEvalFunction (Eval.find c m) (pl,None,lst,line)) m else Eval.add c (Function{rows = [(pl,None,lst,line)]}) m
        | Keyword(c,d) -> if Eval.mem c m then Eval.add c (updateEvalPredicate (Eval.find c m) (d,None,line)) m else Eval.add c (Predicate{rows = [(d,None,line)]}) m
      )
  | Rule(line,a,b,c) -> let m = loop_rules newTypes c in (
    match a with
      Atom(d,e) -> 
      let (lst, pl) = getLastParam e in 
        if Eval.mem d m then Eval.add d (updateEvalFunction (Eval.find d m) (pl,b,lst,line)) m else Eval.add d (Function{rows = [(pl,b,lst,line)]}) m
      | Keyword(d,e) -> if Eval.mem d m then Eval.add d (updateEvalPredicate (Eval.find d m) (e,b,line)) m else Eval.add d (Predicate{rows = [(e,b,line)]}) m
    )
  | OcamlEmbedded(s,n) -> let _ = s |> print_endline in loop_rules newTypes n
  | DeclarationType(_,n) -> loop_rules newTypes n
  | PrintType(nxt) -> let _ = newTypes |> print_endline in loop_rules newTypes nxt;;

  (** Stampa una tupla con i valori contenuti *)
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
  
  (** Stampa un parametro *)
  let printParam v = match v with 
    Name v1 -> v1
    | Variable v1 -> "_" ^ v1 
    | TypeS v1 ->  v1
    | Type(v1,v2) -> v1 ^ printTuple v2
  ;;  
  (** Stampa la riga di un predicato *)
  let rec printResult r = match r with
    AtomList(v1,None) -> (
        match v1 with 
          Atom(v3,v4) -> v3 ^ printTuple v4 
          | Keyword(v3,v4) -> v3 ^ printTuple v4
      ) 
    | AtomList(v1,v2) -> (
        match v1 with 
          Atom(v3,v4) -> v3 ^ printTuple v4 ^ " && " ^ printResult v2
          | Keyword(v3,v4) -> v3 ^ printTuple v4 ^ " && " ^ printResult v2
      ) 
    | None -> "true";;
  
  (** Stampa la riga di una effettiva funzione *)
  let rec printResultState al final = match al with
    AtomList(a,nxt) -> (
     match a with 
     Atom(name, pl) -> 
       let (lst, pl1) = getLastParam pl in 
         (
           match lst with 
             Name v1 -> " let " ^ v1 ^ " = " ^ name ^ " " ^ printTuple pl1 ^ " in " ^ printResultState nxt final
             | Variable v1 -> " let " ^ "_" ^ v1 ^ " = " ^ name ^ " " ^ printTuple pl1 ^ " in " ^ printResultState nxt final
             | TypeS v1 ->  " let " ^ v1 ^ " = " ^ name ^ " " ^ printTuple pl1 ^ " in " ^ printResultState nxt final
             | Type(v1,v2) -> "( match " ^ name ^ " " ^ printTuple pl1 ^ " with " ^ v1 ^ " " ^ printTuple v2 ^ " -> " ^ printResultState nxt final ^ " | _ -> failwith(\"Error!\"))"
         )
     | Keyword(name,pl) -> "if " ^ name ^ " " ^ printTuple pl ^ " then " ^ printResultState nxt final ^ " else failwith(\"Error!\")"
   )
   | None -> printParam final;; 

  (** Stampa una nuova riga di un predicato *)
  let printNewRowPredicate a b = match a with 
    (c,d,line) ->  "\n (** Line "^ (line |> string_of_int) ^" *) \n | " ^ printTuple c ^ " -> " ^ printResult d ^ b;;

  (** Stampa una nuova riga di funzione *)
  let printNewRowFunction a b = match a with 
   (c,d,e,line) ->  "\n (** Line "^ (line |> string_of_int) ^" *) \n |  " ^ printTuple c ^ " -> " ^ printResultState d e ^ b;;

  (** Viene stampata la nuova regola differenziandole tra predicato e funzione *)
  let printRules k v = match v with 
    Predicate{rows = a} -> ("\nlet rec " ^ k ^ " istance = match istance with " ^ (List.fold_right printNewRowPredicate a "\n | _ -> false;;")) |> print_endline
    | Function{rows = a} -> ("\nlet rec " ^ k ^ " istance = match istance with " ^ (List.fold_right printNewRowFunction a "\n | _ -> failwith(\"Error!\");;")) |> print_endline;;
  
  (** Itera tutte le regole valutate in modo da stamparle *)
  let print_rules r = Eval.iter printRules r;;