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

@Context(context,String,res,FunContext).

@CompatEnv
##
let compat gamma gamma' at =
    (* Straightorward implementation from the theory: *)
    let fv = snd (term_getannot at) in
        VarSet.for_all (fun v -> (E1.find_opt gamma v) = (E1.find_opt gamma' v)) fv;;
##

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
type_check(C,Parallel(p1,q1),Tok) :- type_check(C,p1,t), type_check(C,q1,t1), @Compat(t,t1).
type_check(C,Replication(p1),t) :- type_check(C,p1,t).
type_check(C,Splitting(x1,x2,t1,v1),tf) :- #t_check(C,t1,t), @Add(context,C,x1,t,E1), @Add(context,E1,x2,t,E2), type_check(E2,v1,tf).
type_check(C,Match(v1,v2,v3),t) :- t_check(C,v1,t1),t_check(C,v2,t2),@Tcompat(t1,t2), type_check(C,v3,t).
type_check(C,IntCase(v1,v2,x,v4),t2) :- #t_check(C,v1,t1), type_check(C,v2,t2), @Add(context,C,x,t1,E1), type_check(E1,v4,t3), @Compat(t2,t3).
type_check(C,Output(t1,ls,t2),t) :- t_check(C,t1,t3), applyOut(C,t3,ls,t_check,subsumption,t), type_check(C,t2,t).
type_check(C,Restriction(id,r,t1),t) :- @Add(context,C,id,r,E1), type_check(E1,t1,t).
type_check(C,Input(m,ls,p),t) :- #t_check(C,m,t1),#applyIn(t1,C,ls,E1), type_check(E1,p,t).
type_check(C,Skd(l,ls,n,p),t) :- #t_check(C,l,l1), #t_check(C,n,n1), #applyDec(n1,l1,C,ls,E1), type_check(E1,p,t).