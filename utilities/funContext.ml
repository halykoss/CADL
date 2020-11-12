open Core

include Hashtbl.Make
(struct
  module T = struct
    type t = string
    let compare x y = String.compare x y
    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make(T)
end)

let get_empty_context ?size = create ~growth_allowed:true ?size:size

let add x t env = let envc = copy env in ignore (add envc ~key:x ~data:t); envc

let find _C _x = match find _C _x with 
  Some t -> t
  | None -> failwith "Error"
