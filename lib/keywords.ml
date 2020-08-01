
let keywords word = match word with
  "@Add" -> Parser.ADDCTX
  | "@Member" -> Parser.MEMBER
  | "@Update" -> Parser.UPDATECTX
  | "@Head" -> Parser.HEAD
  | "@Tail" -> Parser.TAIL
  |  _ -> failwith("Errore nel token");