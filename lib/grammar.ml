(** Modulo di OCaml per gli insiemi *)
module SS = Set.Make(String);;
(** Parametro usato dentro le() delle regole e delle Formule *)
type param = Name of string | Variable of string | Type of string | Lambda of param * param | None;;
(** Lista di parametri definiti come sopra *)
type paramList = ParamList of param * paramList | None;;
(** Fomrula atomica *)
type atom = Atom of string * paramList | AddInCxt of param * param | MemberCtx of param * param | UpdateCtx of param * param | Head of param * param | Tail of param * param;;
(** Lista di formule atomiche *)
type atomList = AtomList of atom * atomList | None;;
(** Lista di input (Regole o Formule) *)
type input = Rule of atom * atomList * input | Formula of atom * input | None;;
(** Mappe in cui conservo la rappresentazione di regole e fatti *)
let formulas = Hashtbl.create 691;;
let rules = Hashtbl.create 691;;
let newCtx = Hashtbl.create 691;;
let newTypes = Hashtbl.create 1;;
let ctx = Hashtbl.create 691;;
(** Analizzo un parametro e lo formatto *)
let rec analyzeParam p = match p with
  Name s -> "\"" ^ s ^"\""
  | Variable s -> String.lowercase_ascii s
  | Lambda(p,p1) -> "Lambda(" ^ (p |> analyzeParam) ^ " , " ^ (p1 |> analyzeParam) ^ ")"
  | Type s -> let _ = (if  not (List.mem s (Hashtbl.find_all newTypes "type")) then Hashtbl.add newTypes "type" s else ()) in s
  | None -> "None";;
(** Trasformo una lista di parametri da ricorsiva a lista di OCaml *)
let rec analyzeParamList pl = match pl with
  ParamList(p,ls) -> (p |> analyzeParam)::analyzeParamList ls
  | None -> [];;
(** Inizio l'analisi di una formula *)
let analyzeFormula name at = match at with
  Atom(name, pl) ->  (name, analyzeParamList pl)
  | AddInCxt(name,_) -> (name |> analyzeParam,[])
  | MemberCtx(key,value) -> ("getFromContext ctx",(key |> analyzeParam)::(value |> analyzeParam)::[])
  | Head(key,value) -> ("matchHeadType",(key |> analyzeParam)::(value |> analyzeParam)::[])
  | Tail(key,value) -> ("matchTailType",(key |> analyzeParam)::(value |> analyzeParam)::[])
  | UpdateCtx(key,value) -> let () = Hashtbl.add newCtx name ((key |> analyzeParam),(value |> analyzeParam)) in ("true",[]);;
(** Aggungo una formula all'hm *)
let addHashTableFormula h s c = match c with
 (d,e) -> let _ = Hashtbl.add h d e in SS.add d s;;
(** Aggungo una regola all'hm *)
 let addHashTableRule h s v = match v with
 (a,b,c) -> let _ = Hashtbl.add h a (b,c) in SS.add a s;;
(** Prendo una lista di formula atomiche, le analizzo e genero una lista di formule valutate*)
let rec analyzeAtomList name al = match al with
 AtomList(a,als) -> ( a |> analyzeFormula name) :: (als |> analyzeAtomList name)
 | None -> [];;
(** Analizzo una regola e restituisco una tupla con nome, parametri di input e lista di formule associate per la veridicità*)
let analyzeRule v rs = match v with
  Atom(name,pl) -> (name,analyzeParamList pl,analyzeAtomList name rs)
  | _ -> failwith("non riconosciuto");;
(** Inizio l'analisi dell'input, restituisco due insieme (uno contiene le formule, l'altro le regole) *)
let rec analyzeInput a = match a with
  Formula(c,d) -> (
    match c with
    Atom(_,_) -> let (s,t) = analyzeInput d in (analyzeFormula "" c |> addHashTableFormula formulas s ,t)
    | AddInCxt(key,value) -> let () =  (Hashtbl.add ctx (key |> analyzeParam) (value |> analyzeParam)) in analyzeInput d 
    | _ -> failwith("Non posso valutare qui se un valore è membro del contesto"))
  | Rule(c,d,e) -> (
    match c with 
      Atom(_,_) -> let (s,t) = analyzeInput e in (s, analyzeRule c d |> addHashTableRule rules t)
      | AddInCxt(_,_) -> failwith("non Posso defire una regola che inizia con un nuovo valore per contesto")
      | _ -> failwith("Non posso valutare qui se un valore è membro del contesto")
    )
  | None ->  (SS.empty,SS.empty);;
(** Questa funzione costruisce una stringa contenente una tupla di Ocaml senza il ( che la precede *)
let rec printTuple (t:string list) = match t with
 x::[] ->  x ^ printTuple []
 | x::xs ->  x ^ "," ^ printTuple xs
 | [] -> ")";;
(** Questa funziona stampa una riga del costrutto match *)
let rec print_row v = match v with
  x::[] -> " (" ^ printTuple x ^ " -> true" |> print_endline
  | x::xs -> let _ = print_row xs in " | (" ^ printTuple x ^ " -> true " |> print_endline
  | _ -> failwith("non riconosciuto")
  ;;
(** Questa funzione stampa (iterando su tutti i valori della hm) la funzione di match associata alla formula *)
let iter_formulas v =
  let _ = "let "^ v ^" istance = match istance with " |> print_endline  in
    let _ = Hashtbl.find_all formulas v |> print_row in
       " | _ -> false;;\n" |> print_endline ;;
(***)
let rec print_new_ctx ls = match ls with
  (key,value)::xs -> "let ctx = ("^key^","^value^")::ctx in " ^ print_new_ctx xs 
  | [] -> "";;
(** Questa funzione restituisce una stringa con l'intestazione della funzione *)
let rec print_signature name v = match v with
  [] -> " = " ^ print_new_ctx (Hashtbl.find_all newCtx name)
  | x::[] -> " " ^  x  ^ print_signature  name []
  | x::xs -> " " ^  x  ^ print_signature name xs
;;
(** Questa funzione costruisce una stringa contenente il corpo della funzione associata alla regola *)
let rec print_and_val v = match v with
[] -> ""
| (x,y)::[] -> x ^ " " ^ (if y != [] then "(" ^ printTuple y else "") ^ print_and_val []
| (x,y)::xs -> x ^ " " ^ (if y != [] then "(" ^ printTuple y else "") ^ " && " ^ print_and_val xs
;;
(** Questa funzione costruisce una stringa contenente il corpo di più regole compattate *)
let rec print_func name v = match v with
  [] -> "let " ^ name
  | (b,c)::[] ->  ([] |> print_func name) ^ print_signature name b ^ "(" ^ print_and_val c ^ ")"
  | (_,c)::xs ->  (xs |> print_func name) ^ " || " ^ "(" ^ print_and_val c ^ ")" ;;
  (** Itero le regole per poterle stampare *)
  let iter_rules v = Hashtbl.find_all rules v |> print_func v |> Printf.printf "%s;;\n";;