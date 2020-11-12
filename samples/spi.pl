#!
open Utilities;;
!#
t ::= Name(string) | Pair(t,t) | Zero | Succ(t) | Ske([t],t) | Var(string)
and tres ::= Tpub | Tsec | Tany.

term ::= Output(t,[t],term) | Input(t,[string],term) | Zero | Parallel(term,term) | Replication(term) 
| Restriction(string,tres,term) | Match(t,t,term) | Splitting(string,string,t,term) | IntCase(t,term,string,term) 
| Skd(t,[string],t,term)
and res ::= Tok.

##

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

##

@Context(context,String,tres,FunContext).

!!
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
!!
@Subsumption(Tany,Tany).
@Subsumption(Tany,_).
@Subsumption(_,Tany).
@Subsumption(Tpub,Tpub).
@Subsumption(Tsec,Tsec).

@Tcompat(Tpub,Tpub).
@Tcompat(Tsec,Tsec).

apply(Tpub,ls,f,v1) :- 
  listNth(ls,0,v1), f(v1,a), listMap(a,ls,ls1), @Fold(andFunc,ls1,true).
apply(Tsec,ls,func,Tpub) :- 
  listNth(ls,0,v1), listNth(ls,1,v2), listNth(ls,2,v3), @Func(Tsec,v1), @Func(Tany,v2), @Func(Tpub,v3).

t_check(E,Var(x),t) :- @Member(context,E,x,t).
t_check(E,Zero,Tpub).
t_check(E,Name(x),t) :- @Member(context,E,x,t).
t_check(E,Succ(m1),t) :- t_check(E,m1,t).
t_check(E,Pair(v1,v2),t1) :- t_check(E,v1,t1),t_check(E,v2,t2), @Subsumption(t1,t2).
t_check(E,Ske(ls,v),t) :- t_check(E,f), listMap(f,ls,ls1), t_check(E,v,t1), apply(t1,ls1,subsumption,t).

free_var(k,Var(x),t) :- varSingleton(x,k,t).
free_var(k,Name(x),t) :- varSingleton(x,k,t).
free_var(k,Zero,t) :- setEmpty(t).
free_var(k,Pair(v1,v2),t) :- free_var(k,v1,t1), free_var(k,v2,t2), union(t1,t2,t).
free_var(k,Succ(x),t) :- free_var(k,x,t).
free_var(k,Ske(v1,v2),t) :- free_var(k,f), listMap(f,v1,t1),  free_var(k,v2,t2), setAdd(t1,v), union(v,t2,t).

free_variables_cps(Replication(r),k,t) :- free_variables_cps(r,k,t).
free_variables_cps(Parallel(t1,t2),k,t) :- free_variables_cps(t1,k,r1), free_variables_cps(t2,k,r2), union(r1,r2,t).
free_variables_cps(Output(t1,tl,tms),k,v) :- free_var(k,t1,ls1), free_var(k,f), listMap(f,tl,vl),
   setAdd(vl,v), free_variables_cps(tms,k,ls3), union(ls1,v,u1), union(u1,ls3,v).
free_variables_cps(Match(t1,t2,tm1),k,v) :- free_var(k,t1,v1), free_var(k,t2,v2),
  free_variables_cps(tm1,k,v3), union(v1,v2,v4), union(v4,v3,v).
free_variables_cps(Input(t1,ls1,t),k,v) :- free_var(k,t1,v1), setAddSingleton(ls1,v2), free_variables_cps(t,k,v3),
  union(v1,v2,v4), union(v4,v3,v).
free_variables_cps(Splitting(s1,s2,t1,tm1),k,v) :- free_var(k,t1,v1), free_variables_cps(tm1,k,v2), union(v1,v2,v3),
  ssAdd(s1,v3,v4), ssAdd(s2,v4,v).
free_variables_cps(IntCase(t1,tm1,s1,tm2),k,v) :- free_variables_cps(tm1,k,v1), free_variables_cps(tm2,k,v2),
  union(v1,v2,v3), ssAdd(s1,v3,v4), free_var(k,t1,v5), union(v4,v5,v).
free_variables_cps(Skd(t1,sl,t2,tm1),k,v) :- free_variables_cps(tm1,k,v1), free_var(k,t1,v2), free_var(k,t2,v3),
   setAddSingleton(sl,v4), union(v1,v2,v5), union(v3,v4,v6), union(v5,v6,v).
free_variables_cps(Restriction(id,tr,trm),k,v) :-  varSingleton(id,k,t1), free_variables_cps(trm,k,t2), union(t1,t2,v).

@Compat(Tok,Tok).

applyOut(E,Tpub,ls,t_check,subsumption,Tpub) :- t_check(E,f3), listMap(f3,ls,ls1), subsumption(Tpub,f2),
  listMap(f2,ls1,ls2), @Fold(andFunc,ls2,true).
applyOut(E,Tsec,ls, t_check, subsumption,Tpub) :- 
  t_check(E,f3), listMap(f3,ls,ls1), listNth(ls1,0,v1), listNth(ls1,1,v2), listNth(ls1,2,v3),
  @Subsumption(Tsec,v1), @Subsumption(Tany,v2), @Subsumption(Tpub,v3).

applyIn(Tpub,E,ls,E1) :- id(Tpub,pub), listMap(pub,ls,t1), foldl2(addcontext,E,ls,t1,E1).
applyIn(Tsec,E,ls,E3) :- listNth(ls,0,v1), listNth(ls,1,v2), listNth(ls,2,v3), @Add(context,E,v1,Tsec,E1), 
  @Add(context,E1,v2,Tany,E2), @Add(context,E2,v3,Tpub,E3).

applyDec(Tpub,l,E,ls,E1) :- id(l,pub), listMap(pub,ls,t1), foldl2(addcontext,E,ls,t1,E1).
applyDec(Tsec,l,E,ls,E4) :- 
  listNth(ls,0,v1), listNth(ls,1,v2), listNth(ls,2,v3), listNth(ls,3,v4), @Add(context,E,v1,Tsec,E1),
  @Add(context,E1,v2,Tany,E2), @Add(context,E2,v3,Tpub,E3), @Add(context,E3,v4,Tany,E4).

type_check(C,Zero,Tok).
type_check(C,Parallel(p1,q1),Tok) :- type_check(C,p1,res), type_check(C,q1,t1), @Compat(res,t1).
type_check(C,Replication(p1),Tok) :- type_check(C,p1,res).
type_check(C,Splitting(x1,x2,t1,v1),Tok) :- #t_check(C,t1,res), @Add(context,C,x1,res,E1), @Add(context,E1,x2,res,E2), type_check(E2,v1,tf).
type_check(C,Match(v1,v2,v3),Tok) :- t_check(C,v1,t1),t_check(C,v2,t2),@Tcompat(t1,t2), type_check(C,v3,res).
type_check(C,IntCase(v1,v2,x,v4),Tok) :- #t_check(C,v1,t1), type_check(C,v2,t2), @Add(context,C,x,t1,E1), type_check(E1,v4,t3), @Compat(t2,t3).
type_check(C,Output(t1,ls,t2),Tok) :- t_check(C,t1,t3), applyOut(C,t3,ls,t_check,subsumption,res), type_check(C,t2,res1).
type_check(C,Restriction(id,r,t1),Tok) :- @Add(context,C,id,r,E1), type_check(E1,t1,res).
type_check(C,Input(m,ls,p),Tok) :- #t_check(C,m,t1),#applyIn(t1,C,ls,E1), type_check(E1,p,res).
type_check(C,Skd(l,ls,n,p),Tok) :- #t_check(C,l,l1), #t_check(C,n,n1), #applyDec(n1,l1,C,ls,E1), type_check(E1,p,res).