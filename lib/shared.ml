type symbol = string [@@deriving show]

module SymTab = struct
  include Map.Make (String)

  let pp pp_a ff st =
    let open Format in
    let pp_print_binding ff (k, v) =
      fprintf ff "@[<hov 1>(%s,@ %a)@]" k pp_a v
    in
    fprintf ff "@[<hov 1>(%a)@]" (pp_print_list pp_print_binding) (to_list st)
end

module SymSet = Set.Make (String)
