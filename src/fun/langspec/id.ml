open Batteries
open FunSpecification.FunSpecification

(* From https://github.com/esumii/min-caml *)
type t = string (* MinCaml *)
type l = L of string

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let rec id_of_typ = function
  | TypUnit -> "u"
  | TypBool -> "b"
  | TypI -> "i"
  | TypF _ -> "f"
  | TypTu _ -> "t"
  | TypList _ -> "l"
  | TypRef _ -> "r"

let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter

let equal = String.equal
