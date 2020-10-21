#!
open Utilities;;
!#

term ::=  Var(string) | Num(int) | Fun(string, res, term) 
    | App(term, term) | Let(string,term,term) 
    | DeclTup([term]) | GetTup(int,term) | Nil(res) 
    | Unit | Bool(bool) | IsNil(res,term) | Cons(res,term,term)
		| Head(res,term) | Tail(res,term) | Fix(term) | Ref(term)
		| Deref(term) | PointerAss(term,term) | IfThen(term,term,term) | Add(term,term) 
    | Equal(term,term) | Sub(term,term)
and res ::=  TypI | TypUnit | TypF(res, res) | TypTu([res]) | TypList(res) | TypBool | TypRef(res).

##
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
##

@Context(context,String,res,FunContext).

!!
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
!!

free_variables_cps(Var(x),k,t) :- varSingleton(x,k,t).
free_variables_cps(Fun(n,t,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(App(e1,e2),k,t3) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3).
free_variables_cps(Let(x,e1,e2),k,t4) :- free_variables_cps(e1,k,t1), free_variables_cps(e2,k,t2), union(t1,t2,t3), remove(x,t3,t4).
free_variables_cps(DeclTup(ls),k,t) :- apply(free_variables_cps,k,f), listMap(f,ls,ls1), fold_left(union,empty,ls1,t).
free_variables_cps(Fix(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Ref(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Deref(e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(IsNil(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Head(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(Tail(rs,e),k,t) :- free_variables_cps(e,k,t).
free_variables_cps(GetTup(idx,e2),k,t) :- free_variables_cps(e2,k,t).
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
type_check(C,GetTup(idx,exp1),t) :- type_check(C,exp1,TypTu(tup)), getIndexTup(idx,tup,t).
type_check(C,Nil(t),TypList(t)).
type_check(C,IsNil(t,exp),TypBool) :- type_check(C,exp,t2), @Compat(t2,TypList(t)).
type_check(C,Cons(t,exp1,exp2),TypList(t)) :- type_check(C,exp1,t1), type_check(C,exp2,t2), @Compat(t,t1), @Compat(TypList(t),t2).
type_check(C,Head(t,exp),t) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Tail(t,exp),TypList(t)) :- type_check(C,exp,t1), @Compat(t1,TypList(t)).
type_check(C,Fix(exp),t2) :- type_check(C,exp,TypF(t1,t2)), @Compat(t1,t2).
type_check(C,Ref(exp),TypRef(t1)) :- type_check(C,exp,t1).
type_check(C,Deref(exp),t) :- type_check(C,exp,TypRef(t)).
type_check(C,PointerAss(exp,exp1),TypUnit) :- type_check(C,exp,TypRef(t)), type_check(C,exp1,t1), @Compat(t,t1).
type_check(C,IfThen(t1,t2,t3),v2) :- type_check(C,t1,v1), @Compat(v1,TypBool), type_check(C,t2,v2), type_check(C,t3,v3), @Compat(v2,v3).
type_check(C,Add(t1,t2),v2) :- type_check(C,t1,v1), type_check(C,t2,v2), @Compat(v1,v2).
type_check(C,Sub(t1,t2),v2) :- type_check(C,t1,v1), type_check(C,t2,v2), @Compat(v1,v2).
type_check(C,Equal(t1,t2),TypBool) :- type_check(C,t1,v1), type_check(C,t2,v2), @Compat(v1,v2).
