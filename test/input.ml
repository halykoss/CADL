(* //TODO: Riportare modifiche sul branch *)
open Generated;;
open Utilities;;
open OUnit2;;

let rec printType v = match v with 
  TypI -> "Int"
  | TypBool -> "Bool"
  | TypUnit -> "Unit"
  | TypF(t1,t2) -> "" ^ printType t1 ^ " -> " ^ printType t2
  | TypList t -> printType t ^ " list"
  | TypTu ([]) -> ""
  | TypTu (x::xs) -> "(" ^ List.fold_left (fun y x -> y ^ " * " ^ (printType x) ) (printType x) xs  ^ ")"
  | TypRef t -> printType t ^ " ref"
  ;;
let value = [
  ("\\x:Bool. let y = 8 in (x,y)",(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"])))),(TypF(TypBool, TypTu [TypBool;TypI])));
  ("(\\x:Bool. let y = 8 in (x,y)) true",(App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true))),(TypTu [TypBool;TypI]));
  ("((\\x:Bool. let y = 8 in (x,y)) true).0",(GetTup(0,App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true)))),TypBool);
  ("\\x,y:Unit, Unit. isNil(Nil[Bool]) ",(Fun("x",TypUnit,Fun("y",TypUnit,IsNil(TypBool,Nil(TypBool))))),(TypF (TypUnit, TypF (TypUnit, TypBool))));
  ("\\x,y:Unit, Unit. Cons[Bool](true,Nil[Bool])",(Fun("x",TypUnit,Fun("y",TypUnit,Cons(TypBool,Bool true,Nil(TypBool))))),(TypF (TypUnit, TypF (TypUnit, TypList TypBool))));
  ("\\x,y:Unit, Unit. Head[Bool](Cons[Bool](true,Nil[Bool]))",(Fun("x",TypUnit,Fun("y",TypUnit,Head(TypBool,Cons(TypBool,Bool true,Nil(TypBool)))))),(TypF (TypUnit, TypF (TypUnit, TypBool))));
  ("\\x,y:Unit, Unit. Tail[Bool](Cons[Bool](true,Nil[Bool]))",(Fun("x",TypUnit,Fun("y",TypUnit,Tail(TypBool,Cons(TypBool,Bool true,Nil(TypBool)))))),(TypF (TypUnit, TypF (TypUnit, TypList TypBool))));
  (
    "Fix(\\f: int -> int. \\x: int. if ( n == 1 ) then 1 else if (n == 2) then 1 else f (n-1) + f(n -2))",
    Fix(Fun("f",TypF(TypI,TypI),Fun("x",TypI,IfThen(Equal(Var "x",Num 1),Num 1,IfThen(Equal(Var "x",Num 2),Num 1,Add(App(Var "f",Sub(Var "x", Num 1)),App(Var "f",Sub(Var "x", Num 2)))))))),
    TypF(TypI,TypI)
    )
];;


let rec startTests t = match t with
    (name,a,b)::xs -> let _ = name |> print_string in let eval = type_check (FunContext.create ()) a in assert_equal b eval;" -> Passed! ~ Type : " ^ printType b |> print_endline;startTests xs
  | [] -> ("Test has been successfully passed!" |> print_endline)
;;

startTests value;;