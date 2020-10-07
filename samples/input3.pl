##
module VarSet = Set.Make(struct
	type t = string
	let compare = String.compare
end);;

module FunContext = Hashtbl.Make(struct
	type t = string
	(* let hash = Hashtbl.hash *)
	let hash = Hashtbl.hash_param max_int max_int
	let equal = String.equal
end);;
##
term ::=  Var(string) | Num(int) | Fun(string, res, term) 
    | App(term, term) | Let(string,term,term) 
    | DeclTup([term]) | GetTup(term,term) | Nil(res) 
    | Unit | Bool(bool) | IsNil(res,term) | Cons(res,term,term)
		| Head(res,term) | Tail(res,term) | Fix(term) | Ref(term)
		| Deref(term) | PointerAss(term,term) | Location(term) 
and res ::=  TypI | TypUnit | TypF(res, res) | TypTu([res]) | TypList(res) | TypBool | TypRef(res).

##
  let getIndexValue value = match value with Num(nr) -> nr | _ -> failwith "Error";;

  let varSingleton v k = k (VarSet.singleton v);;
  let union ls1 ls2 = VarSet.union ls1 ls2;;
  let remove value ls = VarSet.remove value ls;;
  let fold_left f1 v1 v2 = List.fold_left f1 v1 v2;;
  let empty = VarSet.empty;;

  let listMap f ls = List.map f ls;;

  let rec getIndexTup idx tup = match tup with
    | [] -> failwith("Index Error")
    | x::xs -> if idx == 0 then x else getIndexTup (idx -1) xs;;

  let tuplesEquals t1 t2 = 
    if (
        List.length (List.filter ( fun (a,b) -> a != b ) (List.combine t1 t2))
      ) > 0 then
     false else true;;

  let forAllCompare ls1 ls2 = 
    let mapList = List.map (fun (a,b) -> (a == b)) (List.combine ls1 ls2) in
        (List.fold_right (fun a b -> a && b) mapList true)
    ;;

  let apply v1 k e  = v1 e k;;
##

@Context(context,String,res,FunContext).

free_variables_cps(Var(x),k,t) :- varSingleton(x,k,t).
free_variables_cps(Fun(n,t,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(App(e1,e2),k,t3) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3).
free_variables_cps(Let(x,e1,e2),k,t4) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3), remove(x,t3,t4).
free_variables_cps(DeclTup(ls),k,t) :- apply(free_variables_cps,k,f), listMap(f,ls,ls1), fold_left(union,empty,ls1,t).
free_variables_cps(Fix(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Ref(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Deref(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Location(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(IsNil(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Head(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Tail(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(GetTup(e1,e2),k,t3) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3).
free_variables_cps(Cons(rs,e1,e2),k,t3) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3).
free_variables_cps(PointerAss(e1,e2),k,t3) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3).

@Compat(TypI, TypI).
@Compat(TypUnit, TypUnit).
@Compat(TypBool,TypBool).
@Compat(TypRef(v1),TypRef(v2)) :- @Compat(v1,v2).
@Compat(TypList(t1),TypList(t2)) :- @Compat(t1,t2).
@Compat(TypF(t1, t2), TypF(t3,t4)) :- @Compat(t1, t3), @Compat(t2, t4).
@Compat(TypTu(t1),TypTu(t2)) :- forAllCompare(t1,t2).

type_check(C, Unit, TypUnit).
type_check(C,Bool(b),TypBool).
type_check(C, Num(n), TypI).
type_check(C, Var(x), T) :- @Member(context,C, x, T).
type_check(C, Fun(x, t1, e), TypF(t1,t2)) :- @Add(context,C, x, t1, C1), type_check(C1, e, t2).
type_check(C, App(e1, e2), t2) :- type_check(C, e1, TypF(t1, t2)), type_check(C, e2, t3), @Compat(t1,t3).
type_check(C, Let(x,e,e1),t2) :- type_check(C,e,t1), @Add(context,C, x, t1 , C1), type_check(C1, e1, t2).
type_check(C, DeclTup(e1), TypTu(t)) :- type_check(C,t1) ,@ListMap(t1,e1,t).
type_check(C,GetTup(idx,exp1),t) :- type_check(C,exp1,TypTu(tup)), getIndexValue(idx,nr), getIndexTup(nr,tup,t).
type_check(C,Nil(t),TypList(t)).
type_check(C,IsNil(t,exp),TypBool) :- type_check(C,exp,t2), @Compat(t2,TypList(t)).
type_check(C,Cons(t,exp1,exp2),TypList(t)) :- type_check(C,exp1,t1), type_check(C,exp2,t2), @Compat(t,t1), @Compat(TypList(t),t2).
type_check(C,Head(t,exp),t) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Tail(t,exp),TypList(t)) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Fix(exp),t2) :- type_check(C,exp,TypF(t1,t2)), @Compat(t1,t2).
type_check(C,Ref(exp),TypRef(t1)) :- type_check(C,exp,t1).
type_check(C,Deref(exp),t) :- type_check(C,exp,TypRef(t)).
type_check(C,PointerAss(exp,exp1),TypUnit) :- type_check(C,exp,TypRef(t)), type_check(C,exp1,t1), @Compat(t,t1).

