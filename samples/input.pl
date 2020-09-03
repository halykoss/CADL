#* 
module Name = String;;
type name = Name.t;;
*#

tuples ::= Tuples(exp,tuples) | EndS 
and exp ::=  Var(name) | Num(int) | Fun(name, t, exp) 
    | App(exp, exp) | Let(name,exp,exp) 
    | DeclTup(tuples) | GetTup(exp,exp) | Nil(t) 
    | Unit | Bool(bool) | IsNil(t,exp) | Cons(t,exp,exp)
		| Head(t,exp) | Tail(t,exp) | Fix(exp) | Ref(exp)
		| Deref(exp) | PointerAss(exp,exp) | Location(exp) 
and v ::= TypInnerTup(t,v) | End 
and t ::=  TypI | TypUnit |TypF(t, t) | TypTu(v) |TypList(t) | TypBool | TypRef(t).

#*

let id value = value;;

let rec getIndexTup key tup = match (key,tup) with
  | (0 , TypInnerTup(x,y)) -> x
  | (idx,TypInnerTup(x,y)) -> getIndexTup (idx - 1) y
  | (_ , _) -> failwith("Index Error");;
*#

@Context(type_env,Name,t).

@Compat(TypI, TypI).
@Compat(TypUnit, TypUnit).
@Compat(TypBool,TypBool).
@Compat(TypRef(v1),TypRef(v2)) :- @Compat(v1,v2).
@Compat(TypList(t1),TypList(t2)) :- @Compat(t1,t2).
@Compat(TypF(t1, t2), TypF(t3,t4)) :- @Compat(t1, t3), @Compat(t2, t4).
@Compat(TypTu(End),TypTu(End)).
@Compat(TypTu(TypInnerTup(t1,nxt1)),TypTu(TypInnerTup(t2,nxt2))) :- @Compat(t1,t2), @Compat(TypTu(nxt1),TypTu(nxt2)).

type_check(C, Unit, TypUnit).
type_check(C,Bool(b),TypBool).
type_check(C, Num(n), TypI).
type_check(C, Var(x), T) :- @Member(type_env,C, x, T).
type_check(C, Fun(x, t1, e), TypF(t1,t2)) :- @Add(type_env,C, x, t1, C1), type_check(C1, e, t2).
type_check(C, App(e1, e2), t2) :- type_check(C, e1, TypF(t1, t2)), type_check(C, e2, t3), @Compat(t1,t3).
type_check(C, Let(x,e,e1),t2) :- type_check(C,e,t1), @Add(type_env,C, x, t1 , C1), type_check(C1, e1, t2).
type_check(C,DeclTup(EndS),TypTu(End)).
type_check(C, DeclTup(Tuples(exp,tup)), TypTu(TypInnerTup(t1, t2))) :- type_check(C,exp,t1),type_check(C,DeclTup(tup),TypTu(t2)).
type_check(C,GetTup(idx,exp1),t) :- type_check(C,exp1,TypTu(tup)), id(idx,Num(nr)), getIndexTup(nr,tup,t).
type_check(C,Nil(t),TypList(t)).
type_check(C,IsNil(t,exp),TypBool) :- type_check(C,exp,t2), @Compat(t2,TypList(t)).
type_check(C,Cons(t,exp1,exp2),TypList(t)) :- type_check(C,exp1,t1), type_check(C,exp2,t2), @Compat(t,t1), @Compat(TypList(t),t2).
type_check(C,Head(t,exp),t) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Tail(t,exp),TypList(t)) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Fix(exp),t2) :- type_check(C,exp,TypF(t1,t2)), @Compat(t1,t2).
type_check(C,Ref(exp),TypRef(t1)) :- type_check(C,exp,t1).
type_check(C,Deref(exp),t) :- type_check(C,exp,TypRef(t)).
type_check(C,PointerAss(exp,exp1),TypUnit) :- type_check(C,exp,TypRef(t)), type_check(C,exp1,t1), @Compat(t,t1).

