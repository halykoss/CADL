include Hashtbl.Make
(struct
    type t = string
    let compare = compare
    let hash = Hashtbl.hash
    let equal = String.equal
end)

let empty () = create 4096
let mem k env = mem env k
let find k env = find env k
let find_opt k env = find_opt env k

let add x t env = let envc = copy env in add envc x t; envc
let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
