
let keywords word = match word with
  "@add" -> Parser.ADDCTX
  | "@member" -> Parser.MEMBER
  | "@update" -> Parser.UPDATECTX
  | "@head" -> Parser.HEAD
  | "@tail" -> Parser.TAIL
  |  _ -> failwith("Errore nel token");