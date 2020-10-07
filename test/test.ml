open Generated;;
open OUnit2;;

(* \x:Bool. let y = 8 in (x,y) *)
let t1 = type_check (FunContext.create 10) (Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))));;
assert_equal (TypF(TypBool, TypTu [TypBool;TypI])) t1;;
(* (\x:Bool. let y = 8 in (x,y)) true *)
let t2 = type_check (FunContext.create 10) (App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true)));;
assert_equal (TypTu [TypBool;TypI]) t2;;
(* ((\x:Bool. let y = 8 in (x,y)) true).0 *)
let t3 = type_check (FunContext.create 10) (GetTup(Num 0,App(Fun("x", TypBool,Let("y",Num 8,DeclTup([Var "x";Var "y"]))),Bool(true))));;
assert_equal (TypBool) t3;;
(* \x,y:Unit, Unit. isNil(Nil[Bool]) *)
let t4 = type_check (FunContext.create 10) (Fun("x",TypUnit,Fun("y",TypUnit,IsNil(TypBool,Nil(TypBool)))));;
assert_equal (TypF (TypUnit, TypF (TypUnit, TypBool))) t4;;
(* \x,y:Unit, Unit. Cons[Bool](true,Nil[Bool]) *)
let t5 = type_check (FunContext.create 10) (Fun("x",TypUnit,Fun("y",TypUnit,Cons(TypBool,Bool true,Nil(TypBool)))));;
assert_equal (TypF (TypUnit, TypF (TypUnit, TypList TypBool))) t5;;
(* \x,y:Unit, Unit. Head[Bool](Cons[Bool](true,Nil[Bool])) *)
let t6 = type_check (FunContext.create 10) (Fun("x",TypUnit,Fun("y",TypUnit,Head(TypBool,Cons(TypBool,Bool true,Nil(TypBool))))));;
assert_equal (TypF (TypUnit, TypF (TypUnit, TypBool))) t6;;
(* \x,y:Unit, Unit. Tail[Bool](Cons[Bool](true,Nil[Bool])) *)
let t7 = type_check (FunContext.create 10) (Fun("x",TypUnit,Fun("y",TypUnit,Tail(TypBool,Cons(TypBool,Bool true,Nil(TypBool))))));;
assert_equal (TypF (TypUnit, TypF (TypUnit, TypList TypBool))) t7;;