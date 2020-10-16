
module FunSpecification (* : LanguageSpecification *) = struct

	type t = 
		 | Name of string
		 | Pair of t * t
		 | Zero
		 | Succ of t
		 | Ske of t list * t
		 | Var of string

	 and tres = 
		 | Tpub
		 | Tsec
		 | Tany;;

	type  'a term = 
		 | Output of t * t list *  'a term *  'a 
		 | Input of t * string list *  'a term *  'a 
		 | Zero of 'a 
		 | Parallel of  'a term *  'a term *  'a 
		 | Replication of  'a term *  'a 
		 | Restriction of string * tres *  'a term *  'a 
		 | Match of t * t *  'a term *  'a 
		 | Splitting of string * string * t *  'a term *  'a 
		 | IntCase of t *  'a term * string *  'a term *  'a 
		 | Skd of t * string list * t *  'a term *  'a 

	 and res = 
		 | Tok;;



let listMap f ls = List.map f ls;;
let listFor_all f ls = List.for_all f ls;;
let fold f ls v2 = List.fold_right f ls v2;;
let foldl f ls v2 = List.fold_left f ls v2;;
let foldl2 f ls v2 = List.fold_left2 f ls v2;;
let andFunc v1 v2 = v1 && v2;;
let listNth ls idx = List.nth ls idx;;
let varSingleton v k = k (VarSet.singleton v);;
let id x _ = x;;
let setEmpty = VarSet.empty;;
let union ls1 ls2 = VarSet.union ls1 ls2;;
let ssAdd v ss = VarSet.add v ss;;
let rec setAdd ls = match ls with 
  [] -> VarSet.empty
  | x::xs -> (VarSet.union x (setAdd xs));;
let rec setAddSingleton ls = match ls with 
  [] -> VarSet.empty
  | x::xs -> (VarSet.union (VarSet.singleton x) (setAddSingleton xs));;


type context = tres FunContext.t;;
let membercontext _C x = FunContext.find _C x;;
let addcontext _C x t1 = FunContext.add x t1 _C;;


    let rec type_ppf ppf type_t =
    match type_t with
	Tok -> Format.fprintf ppf "Ok";;
    
    let rec type_t_ppf ppf type_t =
    match type_t with
	Tsec -> Format.fprintf ppf "Secret"
	| Tpub -> Format.fprintf ppf "Public"
	| Tany -> Format.fprintf ppf "Any";;


    let string_of_context (gamma : context) =
        let context_ppf ppf gamma =
            Format.fprintf ppf "[";
            (FunContext.iter (fun id res -> Format.fprintf ppf ", %s |> %a" id type_t_ppf res) gamma);
            Format.fprintf ppf "]";
        in Format.fprintf Format.str_formatter "%a" context_ppf gamma; Format.flush_str_formatter ()


    let string_of_term ppf_annot e : string =
        let rec ppf_term ppf_annot ppf e =
            let ppf_tree = ppf_term ppf_annot in
		match e with
		| Zero(annot)             	  -> Format.fprintf ppf "@[<2>Unit{%a}@]" ppf_annot annot
		| Parallel(e1,e2,annot)		  -> Format.fprintf ppf "@[<2>Parallel(%a,@,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_annot annot
		| Replication(e1,annot)		  -> Format.fprintf ppf "@[<2>Replication(%a){%a}@]" ppf_tree e1 ppf_annot annot
		| Output(_,_,e3,annot)		  -> Format.fprintf ppf "@[<2>Output(t,t list,%a){%a}@]" ppf_tree e3 ppf_annot annot
		| Input(_,_,e3,annot)		  -> Format.fprintf ppf "@[<2>Input(t,string list,%a){%a}@]" ppf_tree e3 ppf_annot annot
		| Restriction(_,e2,e3,annot)      -> Format.fprintf ppf "@[<2>Restriction(string,%a,%a){%a}@]" type_t_ppf e2  ppf_tree e3 ppf_annot annot
		| Match(_,_,e3,annot)		  -> Format.fprintf ppf "@[<2>Input(t,t, %a){%a}@]" ppf_tree e3 ppf_annot annot
		| Splitting(x1,x2,_,e3,annot)     -> Format.fprintf ppf "@[<2>Splitting(%s,%s,t,%a){%a}@]" x1 x2 ppf_tree e3 ppf_annot annot
		| IntCase(_,e1,_,e2,annot)        -> Format.fprintf ppf "@[<2>IntCase(t,%a,string,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_annot annot
		| Skd(_,_,_,e2,annot)             -> Format.fprintf ppf "@[<2>Skd(t,string list,t,%a){%a}@]" ppf_tree e2 ppf_annot annot
    
        in
            ppf_term ppf_annot Format.str_formatter e;
            Format.flush_str_formatter ()

    (* Use the pretty printer to extract string from a type *)
    let string_of_type (t : res) = Format.fprintf Format.str_formatter "%a" type_ppf t; Format.flush_str_formatter ()


let [@warning "-all"] rec apply _e0 _e1 _e2  = match (_e0,_e1,_e2) with 
 (* Line 91 *) 
 |  (Tpub , ls , f) ->  let v1 = listNth  ls 0  in  let a = f  v1  in  let ls1 = listMap  a ls  in if fold  andFunc ls1 true  then v1 else failwith("Error!")
 (* Line 93 *) 
 |  (Tsec , ls , func) ->  let v1 = listNth  ls 0  in  let v2 = listNth  ls 1  in  let v3 = listNth  ls 2  in if func  Tsec v1  then if func  Tany v2  then if func  Tpub v3  then Tpub else failwith("Error!") else failwith("Error!") else failwith("Error!")
 | _ -> failwith("Error!");;

let [@warning "-all"] rec applyDec _e0 _e1 _e2 _e3  = match (_e0,_e1,_e2,_e3) with 
 (* Line 137 *) 
 |  (Tpub , l , _E , ls) ->  let pub = id  l  in  let t1 = listMap  pub ls  in  let _E1 = foldl2  addcontext _E ls t1  in _E1
 (* Line 140 *) 
 |  (Tsec , l , _E , ls) ->  let v1 = listNth  ls 0  in  let v2 = listNth  ls 1  in  let v3 = listNth  ls 2  in  let v4 = listNth  ls 3  in  let _E1 = addcontext _E v1 Tsec in  let _E2 = addcontext _E1 v2 Tany in  let _E3 = addcontext _E2 v3 Tpub in  let _E4 = addcontext _E3 v4 Tany in _E4
 | _ -> failwith("Error!");;

let [@warning "-all"] rec applyIn _e0 _e1 _e2  = match (_e0,_e1,_e2) with 
 (* Line 133 *) 
 |  (Tpub , _E , ls) ->  let pub = id  Tpub  in  let t1 = listMap  pub ls  in  let _E1 = foldl2  addcontext _E ls t1  in _E1
 (* Line 135 *) 
 |  (Tsec , _E , ls) ->  let v1 = listNth  ls 0  in  let v2 = listNth  ls 1  in  let v3 = listNth  ls 2  in  let _E1 = addcontext _E v1 Tsec in  let _E2 = addcontext _E1 v2 Tany in  let _E3 = addcontext _E2 v3 Tpub in _E3
 | _ -> failwith("Error!");;

let [@warning "-all"] rec applyOut _e0 _e1 _e2 _e3 _e4  = match (_e0,_e1,_e2,_e3,_e4) with 
 (* Line 128 *) 
 |  (_E , Tpub , ls , t_check , subsumption) ->  let f3 = t_check  _E  in  let ls1 = listMap  f3 ls  in  let f2 = subsumption  Tpub  in  let ls2 = listMap  f2 ls1  in if fold  andFunc ls2 true  then Tpub else failwith("Error!")
 (* Line 131 *) 
 |  (_E , Tsec , ls , t_check , subsumption) ->  let f3 = t_check  _E  in  let ls1 = listMap  f3 ls  in  let v1 = listNth  ls1 0  in  let v2 = listNth  ls1 1  in  let v3 = listNth  ls1 2  in if subsumption  Tsec v1  then if subsumption  Tany v2  then if subsumption  Tpub v3  then Tpub else failwith("Error!") else failwith("Error!") else failwith("Error!")
 | _ -> failwith("Error!");;

let [@warning "-all"] rec free_var _e0 _e1  = match (_e0,_e1) with 
 (* Line 102 *) 
 |  (k , Var(x)) ->  let t = varSingleton  x k  in t
 (* Line 103 *) 
 |  (k , Name(x)) ->  let t = varSingleton  x k  in t
 (* Line 104 *) 
 |  (k , Zero) ->  let t = setEmpty    in t
 (* Line 105 *) 
 |  (k , Pair(v1 , v2)) ->  let t1 = free_var  k v1  in  let t2 = free_var  k v2  in  let t = union  t1 t2  in t
 (* Line 106 *) 
 |  (k , Succ(x)) ->  let t = free_var  k x  in t
 (* Line 107 *) 
 |  (k , Ske(v1 , v2)) ->  let f = free_var  k  in  let t1 = listMap  f v1  in  let t2 = free_var  k v2  in  let v = setAdd  t1  in  let t = union  v t2  in t
 | _ -> failwith("Error!");;

let [@warning "-all"] rec subsumption _e0 _e1  = match (_e0,_e1) with 
 (* Line 81 *) 
 | (Tany , Tany) -> true
 (* Line 82 *) 
 | (Tany , _) -> true
 (* Line 83 *) 
 | (_ , Tany) -> true
 (* Line 84 *) 
 | (Tpub , Tpub) -> true
 (* Line 85 *) 
 | (Tsec , Tsec) -> true
 | _ -> false;;

let [@warning "-all"] rec t_check _e0 _e1  = match (_e0,_e1) with 
 (* Line 95 *) 
 |  (_E , Var(x)) ->  let t = membercontext _E x in t
 (* Line 96 *) 
 |  (_E , Zero) -> Tpub
 (* Line 97 *) 
 |  (_E , Name(x)) ->  let t = membercontext _E x in t
 (* Line 98 *) 
 |  (_E , Succ(m1)) ->  let t = t_check  _E m1  in t
 (* Line 99 *) 
 |  (_E , Pair(v1 , v2)) ->  let t1 = t_check  _E v1  in  let t2 = t_check  _E v2  in if subsumption  t1 t2  then t1 else failwith("Error!")
 (* Line 100 *) 
 |  (_E , Ske(ls , v)) ->  let f = t_check  _E  in  let ls1 = listMap  f ls  in  let t1 = t_check  _E v  in  let t = apply  t1 ls1 subsumption  in t
 | _ -> failwith("Error!");;

let [@warning "-all"] rec tcompat _e0 _e1  = match (_e0,_e1) with 
 (* Line 87 *) 
 | (Tpub , Tpub) -> true
 (* Line 88 *) 
 | (Tsec , Tsec) -> true
 | _ -> false;;

	let term_getannot t = 
		match t with
			| Output(_,_,_,annot)
			| Input(_,_,_,annot)
			| Zero annot
			| Parallel(_,_,annot)
			| Replication(_,annot)
			| Restriction(_,_,_,annot)
			| Match(_,_,_,annot)
			| Splitting(_,_,_,_,annot)
			| IntCase(_,_,_,_,annot)
			| Skd(_,_,_,_,annot) -> annot;;

	let term_edit (t : 'a term) (ti : ('b term) list) (a : 'b) : ('b term) =
		match (t, ti) with
			| (Output(e0,e1,e2,_), [e2';]) -> Output(e0,e1,e2',a)
			| (Input(e0,e1,e2,_), [e2';]) -> Input(e0,e1,e2',a)
			| (Zero _, []) -> Zero a
			| (Parallel(e0,e1,_), [e0';e1';]) -> Parallel(e0',e1',a)
			| (Replication(e0,_), [e0';]) -> Replication(e0',a)
			| (Restriction(e0,e1,e2,_), [e2';]) -> Restriction(e0,e1,e2',a)
			| (Match(e0,e1,e2,_), [e2';]) -> Match(e0,e1,e2',a)
			| (Splitting(e0,e1,e2,e3,_), [e3';]) -> Splitting(e0,e1,e2,e3',a)
			| (IntCase(e0,e1,e2,e3,_), [e1';e3';]) -> IntCase(e0,e1',e2,e3',a)
			| (Skd(e0,e1,e2,e3,_), [e3';]) -> Skd(e0,e1,e2,e3',a)
			| _ -> failwith("Error");;

	let rec compute_hash e = Hashtbl.hash_param max_int max_int e;;

	let get_sorted_children (t : 'a term) : ((int * 'a term) list) =
		match t with
			| Output(_,_,e2,_) -> [(0,e2);]
			| Input(_,_,e2,_) -> [(0,e2);]
			| Zero _ -> []
			| Parallel(e0,e1,_) -> [(0,e0);(1,e1);]
			| Replication(e0,_) -> [(0,e0);]
			| Restriction(_,_,e2,_) -> [(0,e2);]
			| Match(_,_,e2,_) -> [(0,e2);]
			| Splitting(_,_,_,e3,_) -> [(0,e3);]
			| IntCase(_,e1,_,e3,_) -> [(0,e1);(1,e3);]
			| Skd(_,_,_,e3,_) -> [(0,e3);]
			;;

let compat gamma gamma' at =
    (* Straightorward implementation from the theory: *)
    let fv = snd (term_getannot at) in
        VarSet.for_all (fun v -> (FunContext.find_option gamma v) = (FunContext.find_option gamma' v)) fv;;

let checkjoin (t : (int * VarSet.t) term) (_C : context) (rs : res list) : res option =
		let [@warning "-all"] rec check _e0 _e1  = (match (_e0,_e1) with 
 (* Line 125 *) 
 | (Tok , Tok) -> true
		 | _ -> false)
in match t with

		 (* Line 142 *) 
		 |  (Zero(a)) ->   Some (Tok) 
		 (* Line 143 *) 
		 |  (Parallel(p1 , q1, a)) ->  (match List.nth_opt rs 0 with Some(res) -> ((match List.nth_opt rs 1 with Some(t1) -> ((if check  res t1  then  (  Some (Tok) ) else None))	| None -> None))	| None -> None) 
		 (* Line 144 *) 
		 |  (Replication(p1, a)) ->  (match List.nth_opt rs 0 with Some(res) -> ( Some (Tok))	| None -> None) 
		 (* Line 145 *) 
		 |  (Splitting(x1 , x2 , t1 , v1, a)) ->  (match List.nth_opt rs 0 with Some(tf) -> ( Some (Tok))	| None -> None) 
		 (* Line 146 *) 
		 |  (Match(v1 , v2 , v3, a)) ->  (match t_check  _C v1  with t1 -> (match t_check  _C v2  with t2 -> (if tcompat  t1 t2  then  ( (match List.nth_opt rs 0 with Some(res) -> ( Some (Tok))	| None -> None) ) else None))) 
		 (* Line 147 *) 
		 |  (IntCase(v1 , v2 , x , v4, a)) ->  (match List.nth_opt rs 0 with Some(t2) -> ((match List.nth_opt rs 1 with Some(t3) -> ((if check  t2 t3  then  (  Some (Tok) ) else None))	| None -> None))	| None -> None) 
		 (* Line 148 *) 
		 |  (Output(t1 , ls , t2, a)) ->  (match t_check  _C t1  with t3 -> (match applyOut  _C t3 ls t_check subsumption  with res -> (match List.nth_opt rs 0 with Some(res1) -> ( Some (Tok))	| None -> None))) 
		 (* Line 149 *) 
		 |  (Restriction(id , r , t1, a)) ->  (match List.nth_opt rs 0 with Some(res) -> ( Some (Tok))	| None -> None) 
		 (* Line 150 *) 
		 |  (Input(m , ls , p, a)) ->  (match List.nth_opt rs 0 with Some(res) -> ( Some (Tok))	| None -> None) 
		 (* Line 151 *) 
		 |  (Skd(l , ls , n , p, a)) ->  (match List.nth_opt rs 0 with Some(res) -> ( Some (Tok))	| None -> None) 
		;;


	let compute_fv (e: 'a term) : VarSet.t =
		let [@warning "-all"] rec free_variables_cps _e0 _e1  = (match (_e0,_e1) with 
		 (* Line 109 *) 
		 |  (Replication(r, a) , k) ->  let t = free_variables_cps  r k  in t
		 (* Line 110 *) 
		 |  (Parallel(t1 , t2, a) , k) ->  let r1 = free_variables_cps  t1 k  in  let r2 = free_variables_cps  t2 k  in  let t = union  r1 r2  in t
		 (* Line 112 *) 
		 |  (Output(t1 , tl , tms, a) , k) ->  let ls1 = free_var  k t1  in  let f = free_var  k  in  let vl = listMap  f tl  in  let v = setAdd  vl  in  let ls3 = free_variables_cps  tms k  in  let u1 = union  ls1 v  in  let v = union  u1 ls3  in v
		 (* Line 114 *) 
		 |  (Match(t1 , t2 , tm1, a) , k) ->  let v1 = free_var  k t1  in  let v2 = free_var  k t2  in  let v3 = free_variables_cps  tm1 k  in  let v4 = union  v1 v2  in  let v = union  v4 v3  in v
		 (* Line 116 *) 
		 |  (Input(t1 , ls1 , t, a) , k) ->  let v1 = free_var  k t1  in  let v2 = setAddSingleton  ls1  in  let v3 = free_variables_cps  t k  in  let v4 = union  v1 v2  in  let v = union  v4 v3  in v
		 (* Line 118 *) 
		 |  (Splitting(s1 , s2 , t1 , tm1, a) , k) ->  let v1 = free_var  k t1  in  let v2 = free_variables_cps  tm1 k  in  let v3 = union  v1 v2  in  let v4 = ssAdd  s1 v3  in  let v = ssAdd  s2 v4  in v
		 (* Line 120 *) 
		 |  (IntCase(t1 , tm1 , s1 , tm2, a) , k) ->  let v1 = free_variables_cps  tm1 k  in  let v2 = free_variables_cps  tm2 k  in  let v3 = union  v1 v2  in  let v4 = ssAdd  s1 v3  in  let v5 = free_var  k t1  in  let v = union  v4 v5  in v
		 (* Line 122 *) 
		 |  (Skd(t1 , sl , t2 , tm1, a) , k) ->  let v1 = free_variables_cps  tm1 k  in  let v2 = free_var  k t1  in  let v3 = free_var  k t2  in  let v4 = setAddSingleton  sl  in  let v5 = union  v1 v2  in  let v6 = union  v3 v4  in  let v = union  v5 v6  in v
		 (* Line 123 *) 
		 |  (Restriction(id , tr , trm, a) , k) ->  let t1 = varSingleton  id k  in  let t2 = free_variables_cps  trm k  in  let v = union  t1 t2  in v
		 | _ -> _e1 VarSet.empty)
	in 
	free_variables_cps e (fun d -> d);;

	let tr (i : int) (ti : (int * VarSet.t) term) (t : (int * VarSet.t) term) (_C : context) (rs : res list) : context =
		match t with
		 (* Line 142 *) 
		 |  (Zero(a)) -> failwith("Tr invoked on base case") 
		 (* Line 143 *) 
		 |  (Parallel(p1 , q1, a)) -> _C 
		 (* Line 144 *) 
		 |  (Replication(p1, a)) -> _C 
		 (* Line 145 *) 
		 |  (Splitting(x1 , x2 , t1 , v1, a)) -> (match t_check  _C t1  with res -> let _E1 = addcontext _C x1 res in let _E2 = addcontext _E1 x2 res in (match List.nth_opt rs 0 with Some(tf) -> (failwith "Error") 	| None -> _E2)) 
		 (* Line 146 *) 
		 |  (Match(v1 , v2 , v3, a)) -> _C 
		 (* Line 147 *) 
		 |  (IntCase(v1 , v2 , x , v4, a)) -> (match t_check  _C v1  with t1 -> (match List.nth_opt rs 0 with Some(t2) -> (let _E1 = addcontext _C x t1 in (match List.nth_opt rs 1 with Some(t3) -> (failwith "Error") 	| None -> _E1)) 	| None -> _C)) 
		 (* Line 148 *) 
		 |  (Output(t1 , ls , t2, a)) -> _C 
		 (* Line 149 *) 
		 |  (Restriction(id , r , t1, a)) -> let _E1 = addcontext _C id r in (match List.nth_opt rs 0 with Some(res) -> (failwith "Error") 	| None -> _E1) 
		 (* Line 150 *) 
		 |  (Input(m , ls , p, a)) -> (match t_check  _C m  with t1 -> (match applyIn  t1 _C ls  with _E1 -> (match List.nth_opt rs 0 with Some(res) -> (failwith "Error") 	| None -> _E1))) 
		 (* Line 151 *) 
		 |  (Skd(l , ls , n , p, a)) -> (match t_check  _C l  with l1 -> (match t_check  _C n  with n1 -> (match applyDec  n1 l1 _C ls  with _E1 -> (match List.nth_opt rs 0 with Some(res) -> (failwith "Error") 	| None -> _E1)))) 
		;;
end

