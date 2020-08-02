open Grammar;;

let keywords word v1 v2 = match word with
  "@Add" -> AddInCxt(v1,v2)
  | "@Member" -> MemberCtx(v1,v2)
  | "@Update" -> UpdateCtx(v1,v2)
  | "@Head" -> Head(v1,v2)
  | "@Tail" -> Tail(v1,v2)
  |  _ -> failwith("Errore nel token");