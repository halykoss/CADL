term ::=  Var(string) | Num(int) | Fun(string, res, term) 
    | App(term, term) | Let(string,term,term) 
    | DeclTup([term]) | GetTup(term,term) | Nil(res) 
    | Unit | Bool(bool) | IsNil(res,term) | Cons(res,term,term)
		| Head(res,term) | Tail(res,term) | Fix(term) | Ref(term)
		| Deref(term) | PointerAss(term,term) | Location(term) 
and res ::=  TypI | TypUnit | TypF(res, res) | TypTu([res]) | TypList(res) | TypBool | TypRef(res).

##
  let id value = value;;

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

  let forAllCompare func ls1 ls2 = 
    let mapList = List.map (fun (a,b) -> (func a b)) (List.combine ls1 ls2) in
      if List.mem None mapList then None
      else Some (List.map (
				fun  d -> match d with
					| Some a -> a 
					| None -> failwith("Not here") ) mapList)
    ;;

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
       in Format.fprintf Format.str_formatter "%a" context_ppf gamma; Format.flush_str_formatter ();;

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

check(TypI,TypI,TypI).
check(TypUnit,TypUnit,TypUnit).
check(TypBool,TypBool,TypBool).
check(TypList(t1),TypList(t2),TypList(t)) :- check(t1,t2,t).
check(TypRef(t1),TypRef(t2),TypRef(t)) :- check(t1,t2,t).
check(TypF(h1,b1),TypF(h2,b2),TypF(h,b)) :- check(h1,h2,h), check(b1,b2,b).
check(TypTu(ls1),TypTu(ls2),TypTu(ls)) :- forAllCompare(check,ls1,ls2,ls).

type_check(C, Unit, TypUnit).
type_check(C,Bool(b),TypBool).
type_check(C, Num(n), TypI).
type_check(C, Var(x), T) :- @Member(context,C, x, T).
type_check(C, Fun(x, t1, e), TypF(t1,t2)) :- @Add(context,C, x, t1, C1), type_check(C1, e, t2).
type_check(C, App(e1, e2), t2) :- type_check(C, e1, TypF(t1, t2)), type_check(C, e2, t3), @Compat(t1,t3).
type_check(C, Let(x,e,e1),t2) :- type_check(C,e,t1), @Add(context,C, x, t1 , C1), type_check(C1, e1, t2).
type_check(C, DeclTup(e1), TypTu(t)) :- type_check(C,t1) ,listMap(t1,e1,t).
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

