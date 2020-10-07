
  let rec type_ppf ppf type_t =
    match type_t with
    | TypUnit -> Format.fprintf ppf "unit"
    | TypI -> Format.fprintf ppf "int"
    | TypBool -> Format.fprintf ppf "bool"
    | TypList(t) -> Format.fprintf ppf "[%a]" type_ppf t
    | TypTu(ts) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf ts;
    Format.fprintf ppf "@]"
    | TypF(args,rt) -> Format.fprintf ppf "@[<2>";
    type_ppf ppf args;
    Format.fprintf ppf " -> %a@]" type_ppf rt
    | TypRef r -> Format.fprintf ppf "@[<2>";
    type_ppf ppf r;
  and typelist_syntax_ppf ppf ts =
    Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts;;

  let string_of_context (gamma : context) =
       let context_ppf ppf gamma =
           Format.fprintf ppf "[";
           (FunContext.iter (fun id res -> Format.fprintf ppf ", %s |> %a" id type_ppf res) gamma);
           Format.fprintf ppf "]";
       in Format.fprintf Format.str_formatter "%a" context_ppf gamma; Format.flush_str_formatter ();;
