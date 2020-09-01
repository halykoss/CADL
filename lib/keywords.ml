open Grammar;;
(* La riga a cui mi trovo*)
let num_lines = ref 0;;

let keywords v1 v2 = match v1 with
  "context" -> Context v2
  | "add" -> Add v2
  | "member" -> Member v2
  |  _ -> Keyword(v1,v2);;