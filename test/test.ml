open Generated;;
open OUnit2;;

let value = [
  (* \x:Bool. let y = 8 in (x,y) *)
  ((Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"])))),(TypF(TypBool, TypTu [TypBool;TypI])));
  (* (\x:Bool. let y = 8 in (x,y)) true *)
  ((App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true))),(TypTu [TypBool;TypI]));
  (* ((\x:Bool. let y = 8 in (x,y)) true).0 *)
  ((GetTup(0,App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true)))),TypBool);
  (* \x,y:Unit, Unit. isNil(Nil[Bool]) *)
  ((Fun("x",TypUnit,Fun("y",TypUnit,IsNil(TypBool,Nil(TypBool))))),(TypF (TypUnit, TypF (TypUnit, TypBool))));
  (* \x,y:Unit, Unit. Cons[Bool](true,Nil[Bool]) *)
  ((Fun("x",TypUnit,Fun("y",TypUnit,Cons(TypBool,Bool true,Nil(TypBool))))),(TypF (TypUnit, TypF (TypUnit, TypList TypBool))));
  (* \x,y:Unit, Unit. Head[Bool](Cons[Bool](true,Nil[Bool])) *)
  ((Fun("x",TypUnit,Fun("y",TypUnit,Head(TypBool,Cons(TypBool,Bool true,Nil(TypBool)))))),(TypF (TypUnit, TypF (TypUnit, TypBool))));
  (* \x,y:Unit, Unit. Tail[Bool](Cons[Bool](true,Nil[Bool])) *)
  ((Fun("x",TypUnit,Fun("y",TypUnit,Tail(TypBool,Cons(TypBool,Bool true,Nil(TypBool)))))),(TypF (TypUnit, TypF (TypUnit, TypList TypBool))))
];;


let rec startTests t = match t with
    (a,b)::xs -> let eval = type_check (FunContext.create 10) a in assert_equal b eval;startTests xs
  | [] -> ("Test has been successfully passed!" |> print_endline)
;;

startTests value;;