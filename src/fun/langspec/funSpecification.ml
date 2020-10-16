
module FunSpecification (* : LanguageSpecification *) = struct

	type  'a term = 
		 | Var of string *  'a 
		 | Num of int *  'a 
		 | Fun of string * res *  'a term *  'a 
		 | App of  'a term *  'a term *  'a 
		 | Let of string *  'a term *  'a term *  'a 
		 | DeclTup of  'a term list *  'a 
		 | GetTup of int *  'a term *  'a 
		 | Nil of res *  'a 
		 | Unit of 'a 
		 | Bool of bool *  'a 
		 | IsNil of res *  'a term *  'a 
		 | Cons of res *  'a term *  'a term *  'a 
		 | Head of res *  'a term *  'a 
		 | Tail of res *  'a term *  'a 
		 | Fix of  'a term *  'a 
		 | Ref of  'a term *  'a 
		 | Deref of  'a term *  'a 
		 | PointerAss of  'a term *  'a term *  'a 

	 and res = 
		 | TypI
		 | TypUnit
		 | TypF of res * res
		 | TypTu of res list
		 | TypList of res
		 | TypBool
		 | TypRef of res;;


  let varSingleton v k = k (VarSet.singleton v);;
  let union ls1 ls2 = VarSet.union ls1 ls2;;
  let remove ss x = VarSet.remove ss x;;
  let fold_left f1 v1 v2 = List.fold_left f1 v1 v2;;
  let empty = VarSet.empty;;
	let rec enumerate start final = if final == 0 then [] else (start+1) :: (enumerate (start + 1) (final - 1));;

  let listMap f ls = List.map f ls;;

  let rec getIndexTup idx tup = match tup with
    | [] -> failwith("Index Error")
    | x::xs -> if idx == 0 then x else getIndexTup (idx -1) xs;;

  let tuplesEquals t1 t2 = 
    if (
        List.length (List.filter ( fun (a,b) -> (a <> b) ) (List.combine t1 t2))
      ) > 0 then
     false else true;;

  let forAllCompare ls1 ls2 = 
    let mapList = List.map (fun (a,b) -> (a == b)) (List.combine ls1 ls2) in
        (List.fold_right (fun a b -> a && b) mapList true)
    ;;

  let apply v1 k e  = v1 e k;;

type context = res FunContext.t;;
let membercontext _C x = FunContext.find _C x;;
let addcontext _C x t1 = FunContext.add x t1 _C;;


    let rec type_ppf ppf type_t =
    match type_t with
    | TypUnit -> Format.fprintf ppf "unit"
    | TypI -> Format.fprintf ppf "int"
    | TypBool -> Format.fprintf ppf "bool"
    | TypList(t) -> Format.fprintf ppf "[%a]" type_ppf t
    | TypTu(ts) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf ts;
    Format.fprintf ppf "@]"
    | TypF(args,rt) -> Format.fprintf ppf "@[<2>";
    type_ppf ppf args;
    Format.fprintf ppf " -> %a@]" type_ppf rt
    | TypRef r -> Format.fprintf ppf "@[<2>";
    type_ppf ppf r;
  and typelist_syntax_ppf ppf ts =
    Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts;;
    


    let string_of_context (gamma : context) =
        let context_ppf ppf gamma =
            Format.fprintf ppf "[";
            (FunContext.iter (fun id res -> Format.fprintf ppf ", %s |> %a" id type_ppf res) gamma);
            Format.fprintf ppf "]";
        in Format.fprintf Format.str_formatter "%a" context_ppf gamma; Format.flush_str_formatter ()


    let string_of_term ppf_annot e : string =
        let rec ppf_term ppf_annot ppf e =
            let ppf_tree = ppf_term ppf_annot in
		match e with
		| Unit(annot)             	  -> Format.fprintf ppf "@[<2>Unit{%a}@]" ppf_annot annot
		| Bool(b, annot)         	    -> Format.fprintf ppf "@[<2>%b{%a}@]" b ppf_annot annot
		| Num(n, annot)          	    -> Format.fprintf ppf "@[<2>%d{%a}@]" n ppf_annot annot
		| Let(id, e1, e2, annot)      -> Format.fprintf ppf "@[<2>Let(%s,%a,@,%a){%a}@]" id ppf_tree e1 ppf_tree e2 ppf_annot annot
		| Var(id, annot)        	    -> Format.fprintf ppf "@[<2>Var(%s){%a}@]" id  ppf_annot annot
		| DeclTup(e1, annot)     		-> Format.fprintf ppf "@[<2>Tuple(";
																			list_syntax_ppf ppf_tree ppf e1;
																			Format.fprintf ppf "){%a}@]" ppf_annot annot
		| GetTup(id,e2, annot)        -> Format.fprintf ppf "@[<2>GetTup(%d,%a){%a}@]" id ppf_tree e2 ppf_annot annot
		| Nil(_,annot)								-> Format.fprintf ppf "@[<2>Nil(res){%a}@]" ppf_annot annot
		| Fun(id,_,e1,annot)					-> Format.fprintf ppf "@[<2>Fun(%s,%a){%a}@]" id ppf_tree e1 ppf_annot annot
		| App(e, es, annot)           -> Format.fprintf ppf "@[<2>App(%a,@,%a){%a}@]" ppf_tree e ppf_tree es ppf_annot annot
		| _ -> Format.fprintf ppf "Something"

        and list_syntax_ppf syntax_ppf ppf es =
        Format.pp_print_list syntax_ppf ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ") ppf es

        in
            ppf_term ppf_annot Format.str_formatter e;
            Format.flush_str_formatter ()

    (* Use the pretty printer to extract string from a type *)
    let string_of_type (t : res) = Format.fprintf Format.str_formatter "%a" type_ppf t; Format.flush_str_formatter ()


	let term_getannot t = 
		match t with
			| Var(_,annot)
			| Num(_,annot)
			| Fun(_,_,_,annot)
			| App(_,_,annot)
			| Let(_,_,_,annot)
			| DeclTup(_,annot)
			| GetTup(_,_,annot)
			| Nil(_,annot)
			| Unit annot
			| Bool(_,annot)
			| IsNil(_,_,annot)
			| Cons(_,_,_,annot)
			| Head(_,_,annot)
			| Tail(_,_,annot)
			| Fix(_,annot)
			| Ref(_,annot)
			| Deref(_,annot)
			| PointerAss(_,_,annot) -> annot;;

	let term_edit (t : 'a term) (ti : ('b term) list) (a : 'b) : ('b term) =
		match (t, ti) with
			| (Var(e0,_), []) -> Var(e0,a)
			| (Num(e0,_), []) -> Num(e0,a)
			| (Fun(e0,e1,e2,_), [e2';]) -> Fun(e0,e1,e2',a)
			| (App(e0,e1,_), [e0';e1';]) -> App(e0',e1',a)
			| (Let(e0,e1,e2,_), [e1';e2';]) -> Let(e0,e1',e2',a)
			| (DeclTup(e0,_), _) -> DeclTup(ti,a)
			| (GetTup(e0,e1,_), [e1';]) -> GetTup(e0,e1',a)
			| (Nil(e0,_), []) -> Nil(e0,a)
			| (Unit _, []) -> Unit a
			| (Bool(e0,_), []) -> Bool(e0,a)
			| (IsNil(e0,e1,_), [e1';]) -> IsNil(e0,e1',a)
			| (Cons(e0,e1,e2,_), [e1';e2';]) -> Cons(e0,e1',e2',a)
			| (Head(e0,e1,_), [e1';]) -> Head(e0,e1',a)
			| (Tail(e0,e1,_), [e1';]) -> Tail(e0,e1',a)
			| (Fix(e0,_), [e0';]) -> Fix(e0',a)
			| (Ref(e0,_), [e0';]) -> Ref(e0',a)
			| (Deref(e0,_), [e0';]) -> Deref(e0',a)
			| (PointerAss(e0,e1,_), [e0';e1';]) -> PointerAss(e0',e1',a)
			| _ -> failwith("Error");;

	let rec compute_hash e = Hashtbl.hash_param max_int max_int e;;

	let get_sorted_children (t : 'a term) : ((int * 'a term) list) =
		match t with
			| Var(_,_) -> []
			| Num(_,_) -> []
			| Fun(_,_,e2,_) -> [(0,e2);]
			| App(e0,e1,_) -> [(0,e0);(1,e1);]
			| Let(_,e1,e2,_) -> [(0,e1);(1,e2);]
			| DeclTup(e0,_) -> [] @ ( (List.combine (enumerate 0 (List.length e0)) e0)) @ []
			| GetTup(_,e1,_) -> [(0,e1);]
			| Nil(_,_) -> []
			| Unit _ -> []
			| Bool(_,_) -> []
			| IsNil(_,e1,_) -> [(0,e1);]
			| Cons(_,e1,e2,_) -> [(0,e1);(1,e2);]
			| Head(_,e1,_) -> [(0,e1);]
			| Tail(_,e1,_) -> [(0,e1);]
			| Fix(e0,_) -> [(0,e0);]
			| Ref(e0,_) -> [(0,e0);]
			| Deref(e0,_) -> [(0,e0);]
			| PointerAss(e0,e1,_) -> [(0,e0);(1,e1);]
			;;

let compat gamma gamma' at =
    (* Straightorward implementation from the theory: *)
    let fv = snd (term_getannot at) in
        VarSet.for_all (fun v -> (FunContext.find_option gamma v) = (FunContext.find_option gamma' v)) fv;;

let checkjoin (t : (int * VarSet.t) term) (_C : context) (rs : res list) : res option =
		let [@warning "-all"] rec check _e0 _e1  = (match (_e0,_e1) with 
 (* Line 117 *) 
 | (TypI , TypI) -> true
 (* Line 118 *) 
 | (TypUnit , TypUnit) -> true
 (* Line 119 *) 
 | (TypBool , TypBool) -> true
 (* Line 120 *) 
 | (TypRef(v1) , TypRef(v2)) -> check v1 v2 
 (* Line 121 *) 
 | (TypList(t1) , TypList(t2)) -> check t1 t2 
 (* Line 122 *) 
 | (TypF(t1 , t2) , TypF(t3 , t4)) -> check t1 t3  && check t2 t4 
 (* Line 123 *) 
 | (TypTu(t1) , TypTu(t2)) -> forAllCompare t1 t2 
		 | _ -> false)
in match t with

		 (* Line 125 *) 
		 |  (Unit(a)) ->   Some (TypUnit) 
		 (* Line 126 *) 
		 |  (Bool(b, a)) ->   Some (TypBool) 
		 (* Line 127 *) 
		 |  (Num(n, a)) ->   Some (TypI) 
		 (* Line 128 *) 
		 |  (Var(x, a)) ->  (match membercontext _C x with _T ->  Some (_T)) 
		 (* Line 129 *) 
		 |  (Fun(x , t1 , e, a)) ->  (match List.nth_opt rs 0 with Some(t2) -> ( Some (TypF(t1 , t2)))	| None -> None) 
		 (* Line 130 *) 
		 |  (App(e1 , e2, a)) ->  (match List.nth_opt rs 0 with Some(TypF(t1 , t2)) -> ((match List.nth_opt rs 1 with Some(t3) -> ((if check  t1 t3  then  (  Some (t2) ) else None))	| None -> None)) 	| Some _ -> None 	| None -> None) 
		 (* Line 131 *) 
		 |  (Let(x , e , e1, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ((match List.nth_opt rs 1 with Some(t2) -> ( Some (t2))	| None -> None))	| None -> None) 
		 (* Line 132 *) 
		 |  (DeclTup(e1, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ((match rs with t ->  Some (TypTu(t))))	| None -> None) 
		 (* Line 133 *) 
		 |  (GetTup(idx , exp1, a)) ->  (match List.nth_opt rs 0 with Some(TypTu(tup)) -> ((match getIndexTup  idx tup  with t ->  Some (t))) 	| Some _ -> None 	| None -> None) 
		 (* Line 134 *) 
		 |  (Nil(t, a)) ->   Some (TypList(t)) 
		 (* Line 135 *) 
		 |  (IsNil(t , exp, a)) ->  (match List.nth_opt rs 0 with Some(t2) -> ((if check  t2 (TypList(t))  then  (  Some (TypBool) ) else None))	| None -> None) 
		 (* Line 136 *) 
		 |  (Cons(t , exp1 , exp2, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ((match List.nth_opt rs 1 with Some(t2) -> ((if check  t t1  then  ( (if check  (TypList(t)) t2  then  (  Some (TypList(t)) ) else None) ) else None))	| None -> None))	| None -> None) 
		 (* Line 137 *) 
		 |  (Head(t , exp, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ((if check  t1 (TypList(t))  then  (  Some (t) ) else None))	| None -> None) 
		 (* Line 138 *) 
		 |  (Tail(t , exp, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ((if check  t1 (TypList(t))  then  (  Some (TypList(t)) ) else None))	| None -> None) 
		 (* Line 139 *) 
		 |  (Fix(exp, a)) ->  (match List.nth_opt rs 0 with Some(TypF(t1 , t2)) -> ((if check  t1 t2  then  (  Some (t2) ) else None)) 	| Some _ -> None 	| None -> None) 
		 (* Line 140 *) 
		 |  (Ref(exp, a)) ->  (match List.nth_opt rs 0 with Some(t1) -> ( Some (TypRef(t1)))	| None -> None) 
		 (* Line 141 *) 
		 |  (Deref(exp, a)) ->  (match List.nth_opt rs 0 with Some(TypRef(t)) -> ( Some (t)) 	| Some _ -> None 	| None -> None) 
		 (* Line 142 *) 
		 |  (PointerAss(exp , exp1, a)) ->  (match List.nth_opt rs 0 with Some(TypRef(t)) -> ((match List.nth_opt rs 1 with Some(t1) -> ((if check  t t1  then  (  Some (TypUnit) ) else None))	| None -> None)) 	| Some _ -> None 	| None -> None) 
		;;


	let compute_fv (e: 'a term) : VarSet.t =
		let [@warning "-all"] rec free_variables_cps _e0 _e1  = (match (_e0,_e1) with 
		 (* Line 102 *) 
		 |  (Var(x, a) , k) ->  let t = varSingleton  x k  in t
		 (* Line 103 *) 
		 |  (Fun(n , t , e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 104 *) 
		 |  (App(e1 , e2, a) , k) ->  let t1 = free_variables_cps  e1 k  in  let t2 = free_variables_cps  e2 k  in  let t3 = union  t1 t2  in t3
		 (* Line 105 *) 
		 |  (Let(x , e1 , e2, a) , k) ->  let t1 = free_variables_cps  e1 k  in  let t2 = free_variables_cps  e2 k  in  let t3 = union  t1 t2  in  let t4 = remove  x t3  in t4
		 (* Line 106 *) 
		 |  (DeclTup(ls, a) , k) ->  let f = apply  free_variables_cps k  in  let ls1 = listMap  f ls  in  let t = fold_left  union empty ls1  in t
		 (* Line 107 *) 
		 |  (Fix(e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 108 *) 
		 |  (Ref(e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 109 *) 
		 |  (Deref(e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 110 *) 
		 |  (IsNil(rs , e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 111 *) 
		 |  (Head(rs , e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 112 *) 
		 |  (Tail(rs , e, a) , k) ->  let t = free_variables_cps  e k  in t
		 (* Line 113 *) 
		 |  (GetTup(idx , e2, a) , k) ->  let t = free_variables_cps  e2 k  in t
		 (* Line 114 *) 
		 |  (Cons(rs , e1 , e2, a) , k) ->  let t1 = free_variables_cps  e1 k  in  let t2 = free_variables_cps  e2 k  in  let t3 = union  t1 t2  in t3
		 (* Line 115 *) 
		 |  (PointerAss(e1 , e2, a) , k) ->  let t1 = free_variables_cps  e1 k  in  let t2 = free_variables_cps  e2 k  in  let t3 = union  t1 t2  in t3
		 | _ -> _e1 VarSet.empty)
	in 
	free_variables_cps e (fun d -> d);;

	let tr (i : int) (ti : (int * VarSet.t) term) (t : (int * VarSet.t) term) (_C : context) (rs : res list) : context =
		match t with
		 (* Line 125 *) 
		 |  (Unit(a)) -> failwith("Tr invoked on base case") 
		 (* Line 126 *) 
		 |  (Bool(b, a)) -> failwith("Tr invoked on base case") 
		 (* Line 127 *) 
		 |  (Num(n, a)) -> failwith("Tr invoked on base case") 
		 (* Line 128 *) 
		 |  (Var(x, a)) -> failwith("Tr invoked on base case") 
		 (* Line 129 *) 
		 |  (Fun(x , t1 , e, a)) -> let _C1 = addcontext _C x t1 in (match List.nth_opt rs 0 with Some(t2) -> (failwith "Error") 	| None -> _C1) 
		 (* Line 130 *) 
		 |  (App(e1 , e2, a)) -> _C 
		 (* Line 131 *) 
		 |  (Let(x , e , e1, a)) -> (match List.nth_opt rs 0 with Some(t1) -> (let _C1 = addcontext _C x t1 in (match List.nth_opt rs 1 with Some(t2) -> (failwith "Error") 	| None -> _C1)) 	| None -> _C) 
		 (* Line 132 *) 
		 |  (DeclTup(e1, a)) -> _C 
		 (* Line 133 *) 
		 |  (GetTup(idx , exp1, a)) -> _C 
		 (* Line 134 *) 
		 |  (Nil(t, a)) -> failwith("Tr invoked on base case") 
		 (* Line 135 *) 
		 |  (IsNil(t , exp, a)) -> _C 
		 (* Line 136 *) 
		 |  (Cons(t , exp1 , exp2, a)) -> _C 
		 (* Line 137 *) 
		 |  (Head(t , exp, a)) -> _C 
		 (* Line 138 *) 
		 |  (Tail(t , exp, a)) -> _C 
		 (* Line 139 *) 
		 |  (Fix(exp, a)) -> _C 
		 (* Line 140 *) 
		 |  (Ref(exp, a)) -> _C 
		 (* Line 141 *) 
		 |  (Deref(exp, a)) -> _C 
		 (* Line 142 *) 
		 |  (PointerAss(exp , exp1, a)) -> _C 
		;;
end

