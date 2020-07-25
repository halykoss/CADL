(** TODO: sistemare riga 29 
    Modulo di OCaml per gli insiemi *)
module SS = Set.Make(String);;
(** Parametro usato dentro le() delle regole e delle Formule *)
type param = Name of string | Variable of string;;
(** Lista di parametri definiti come sopra *)
type paramList = ParamList of param * paramList | None;;
(** Fomrula atomica *)
type atom = Atom of string * paramList | AddInCxt of string * string;;
(** Lista di formule atomiche *)
type atomList = AtomList of atom * atomList | None;;
(** Lista di input (Regole o Formule) *)
type input = Rule of atom * atomList * input | Formula of atom * input | None;;
(** Mappe in cui conservo la rappresentazione di regole e fatti *)
let formulas = Hashtbl.create 691;;
let rules = Hashtbl.create 691;;
let ctx = Hashtbl.create 691;;
(** Analizzo un parametro e lo formatto *)
let analyzeParam p = match p with
  Name s -> "\"" ^ s ^"\""
  | Variable s -> String.lowercase_ascii s;;
(** Trasformo una lista di parametri da ricorsiva a lista di OCaml *)
let rec analyzeParamList pl = match pl with
  ParamList(p,ls) -> (p |> analyzeParam)::analyzeParamList ls
  | None -> []
;;
(** Inizio l'analisi di una formula *)
let analyzeFormula at = match at with
  Atom(name, pl) ->  (name, analyzeParamList pl)
  | AddInCxt(name,value) -> (name,[]);;
(** Aggungo una formula all'hm *)
let addHashTableFormula h s c = match c with
 (d,e) -> let _ = Hashtbl.add h d e in SS.add d s;;
(** Aggungo una regola all'hm *)
 let addHashTableRule h s v = match v with
 (a,b,c) -> let _ = Hashtbl.add h a (b,c) in SS.add a s;;
(** Prendo una lista di formula atomiche, le analizzo e genero una lista di formule valutate*)
let rec analyzeAtomList al = match al with
 AtomList(a,als) -> ( a |> analyzeFormula)  :: (als |> analyzeAtomList)
 | None -> [];;
(** Analizzo una regola e restituisco una tupla con nome, parametri di input e lista di formule associate per la veridicità*)
let analyzeRule v rs = match v with
  Atom(name,pl) -> (name,analyzeParamList pl,analyzeAtomList rs)
  | _ -> failwith("non riconosciuto");;
(** Inizio l'analisi dell'input, restituisco due insieme (uno contiene le formule, l'altro le regole) *)
let rec analyzeInput a = match a with
  Formula(c,d) -> let (s,t) = analyzeInput d in (analyzeFormula c |> addHashTableFormula formulas s ,t)
  | Rule(c,d,e) -> (
    match c with 
      Atom(_,_) -> let (s,t) = analyzeInput e in (s, analyzeRule c d |> addHashTableRule rules t)
      |AddInCxt(key,value) -> let () =  (Hashtbl.add ctx key value) in analyzeInput e
    )
  | None ->  (SS.empty,SS.empty);;
(** AST di prova *)
let value = Formula(
  (* queen(elizabethII) *)
  AddInCxt("prova","ciao"),
  (* queen(Victoria) *)
  Formula(
    Atom("queen",ParamList(Name("Victoria"),None)),
    Formula(
      (* firstMale(charles) *)
        Atom("firstMale",ParamList(Name("charles"),None)),
        Formula(
          (* mother(elizabethII, charles) *)
          Atom("mother",ParamList(Name("elizabethII"),ParamList(Name("charles"),None))),
          Rule(
            (** british(X) :- queen(X) *)
            AddInCxt("british","type"),
            AtomList(Atom("queen",ParamList(Variable("X"),None)),
              None),
            (** king(X) :- mother(elizabethII,X), firstMale(X) *)
            Rule(Atom("king",ParamList(Variable("X"),None)),
              AtomList(Atom("mother",ParamList(Name("elizabethII"),ParamList(Variable("X"),None))),
              AtomList(Atom("firstMale",ParamList(Variable("X"),None)),
              None)),
            Rule(
              (** british(X) :- mother(elizabethII,X) *)
              Atom("british",ParamList(Variable("X"),None)),
              AtomList(
                Atom("mother",
                  ParamList(Name("elizabethII"),
                  ParamList(Variable("X"),None)))
                ,None),
              None)))
    ))));;
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
(** Questa funzione restituisce una stringa con l'intestazione della funzione *)
let rec print_signature v = match v with
  [] -> " = "
  | x::[] -> " " ^  x  ^ print_signature []
  | x::xs -> " " ^  x  ^ print_signature xs
;;
(** Questa funzione costruisce una stringa contenente il corpo della funzione associata alla regola *)
let rec print_and_val v = match v with
[] -> " "
| (x,y)::[] -> x ^ " " ^ "(" ^ printTuple y ^ print_and_val []
| (x,y)::xs -> x ^ " " ^ "(" ^ printTuple y ^ " && " ^ print_and_val xs
;;
(** Questa funzione costruisce una stringa contenente il corpo di più regole compattate *)
let rec print_func name v = match v with
  [] -> "let " ^ name
  | (b,c)::[] ->  ([] |> print_func name) ^ print_signature b ^ "(" ^ print_and_val c ^ ")"
  | (b,c)::xs ->  (xs |> print_func name) ^ " || " ^ "(" ^ print_and_val c ^ ")" ;;
(** Itero le regole per poterle stampare *)
let iter_rules v = Hashtbl.find_all rules v |> print_func v |> Printf.printf "%s;;\n";;
(***)
(***)
(***)
(** In a ho le formule e in b le regole *)
(***)
(***)
(***)
let (a,b) = analyzeInput value;;
"type ctxVal = CtxVal of string | None;;\n\nlet ctx = [ " |> print_endline;;
Hashtbl.fold (fun k v bo ->  "(\""^k ^"\",\"" ^ v ^ "\")" ^ ";\n" ^ bo  ) ctx "];;\n" |> print_endline;;
SS.iter iter_formulas a;;
SS.iter iter_rules b;;
exit(0);;