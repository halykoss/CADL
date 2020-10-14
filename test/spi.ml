open Utilities;;
open Generated;;
open OUnit2;;

let ctx = (FunContext.create ());;
let ctx = FunContext.add "key" Tsec ctx;;
let ctx = FunContext.add "some" Tany ctx;;
let ctx = FunContext.add "n" Tpub ctx;;

let rec test_ls ls idx = 
  match ls with 
    [] -> "All tests has been successfully passed!" |> print_endline
    | x::xs -> let _ = ( (type_check ctx x) |> (assert_equal Tok)) in let _ = ("Test " ^ string_of_int idx ^ " passed!" |> print_endline) in test_ls xs (idx + 1)
;;

let ls_to_be_tested = [
(* 1. Test per Nil *)
  Zero;
(* 2. Test per Output con chiave secret *)
  Output(Name "key", [Var "key"; Name "some"; Zero], Zero);
(* 3. Test per splitting e Output con chiave public *)
  Splitting("x1","x2",Succ(Zero),Output(Name "n",[Var "x1";Var "x2"], Zero));
(* 4. Test per input chiave pubblica  *)
  Input(Name "n", ["x1";"x2";"x3";"x4"], Output(Name "n", [Var "x1"; Var "x2"; Var "x3"; Var "x4"], Zero));
(* 5. Test per input con chiave segreta e output con chiave segreta *)
  Input(Name "key", ["x1";"x2";"x3"], Output(Name "x1", [Var "x1"; Var "x2"; Var "x3"], Zero));
(* 6. Test per replication *)
  Replication(Input(Name "n", ["x1"], Splitting("x2","x3",Ske([Name "key"],Var "x1"),Output(Var "x2", [Var "x3"; Name "some"; Succ(Zero)], Zero))));
(* 7. Test per processi paralleli *)
  Parallel(
    Input(Name "key", ["x1";"x2";"x3"], Output(Name "x1", [Var "x1"; Var "x2"; Var "x3"], Zero)),
    Replication(Input(Name "n", ["x1"], Splitting("x2","x3",Ske([Name "key"],Var "x1"),Output(Var "x2", [Var "x3"; Name "some"; Succ(Zero)], Zero))))
  );
(* 8. Test per Restriction *)
  Restriction("key2",Tsec, Output(Name "key2", [Var "key"; Name "some"; Zero], Zero));
(* 9. Test per IntCase *)
  IntCase(
    Succ(Succ(Zero)),
    Input(Name "key", ["x1";"x2";"x3"], Output(Name "x1", [Var "x1"; Var "x2"; Var "x3"], Zero)),
    "x",
    Input(Var "x", ["x1";"x2";"x3";"x4"], Output(Var "x", [Var "x1"; Var "x2"; Var "x3"; Var "x4"], Zero))
  );
(* 10. Test per Match con M e N   *)
  Match(
    Succ(Succ(Zero)),
    Succ(Zero),
    Output(Name "key", [Var "key"; Name "some"; Zero], Zero)
  );
(* 11. Test per match con M e N secret *)
  Input(Name "key", ["x1";"x2";"x3"], Match(
    Var "x1",
    Name "key",
    Output(Name "key", [Var "key"; Name "some"; Zero], Zero)
  ));
(* 12. Test per Level Decryption Public *)
  Skd(
    Ske([Succ(Zero);Succ(Succ(Zero));Zero],Zero),
    ["x1";"x2";"x3";"x4"],
    Name "n",
    Output(Name "x1",[Var "x2";Var "x3";Var "x4"], Zero)
  );
(* 13. Test per Level Decryption Public *)
  Skd(
    Zero,
    ["x1";"x2";"x3";"x4"],
    Name "key",
    Output(Name "x1", [Var "x2"; Name "x3"; Name "x4"], Zero)
  );
];;

test_ls ls_to_be_tested 1;;