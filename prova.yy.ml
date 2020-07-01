let firstMale istance = match istance with 
 ("charles") -> true
 | _ -> false;;

let mother istance = match istance with 
 ("elizabethII","charles") -> true
 | _ -> false;;

let queen istance = match istance with 
 ("Victoria") -> true
 | ("elizabethII") -> true 
 | _ -> false;;

let british x = (mother ("elizabethII",x) ) || (queen (x) );;
let king x = (mother ("elizabethII",x) && firstMale (x) );;
