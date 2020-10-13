
include Set.Make(struct
	type t = string
	let compare = String.compare
end);;

let remove ss x = remove ss x;;