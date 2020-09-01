(** \x:Bool. let y = 8 in (x,y) *)
type_check E1.empty (Fun("x", TypBool,Let("y",Num 8,DeclTup(Tuples(Var "x",Tuples(Var "y",EndS))))));;
(** (\x:Bool. let y = 8 in (x,y)) true *)
type_check E1.empty (App(Fun("x", TypBool,Let("y",Num 8,DeclTup(Tuples(Var "x",Tuples(Var "y",EndS))))),Bool(true)));;
(** ((\x:Bool. let y = 8 in (x,y)) true).0 *)
type_check E1.empty (GetTup(Num 0,App(Fun("x", TypBool,Let("y",Num 8,DeclTup(Tuples(Var "x",Tuples(Var "y",EndS))))),Bool(true))));;
(** \x,y:Unit, Unit. isNil(Nil[Bool]) *)
type_check E1.empty (Fun("x",TypUnit,Fun("y",TypUnit,IsNil(TypBool,Nil(TypBool)))));;
(** \x,y:Unit, Unit. Cons[Bool](true,Nil[Bool]) *)
type_check E1.empty (Fun("x",TypUnit,Fun("y",TypUnit,Cons(TypBool,Bool true,Nil(TypBool)))));;
(** \x,y:Unit, Unit. Head[Bool](Cons[Bool](true,Nil[Bool])) *)
type_check E1.empty (Fun("x",TypUnit,Fun("y",TypUnit,Head(TypBool,Cons(TypBool,Bool true,Nil(TypBool))))));;
(** \x,y:Unit, Unit. Tail[Bool](Cons[Bool](true,Nil[Bool])) *)
type_check E1.empty (Fun("x",TypUnit,Fun("y",TypUnit,Tail(TypBool,Cons(TypBool,Bool true,Nil(TypBool))))));;