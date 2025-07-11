open Shared

type t =
  | Laz of t
  | Let of symbol * t * t
  | Fix of (string * t) list * t
  | Cnd of t * t * t
  | Lam of symbol * t
  | App of t * t
  | Var of symbol
  | LitInt of int
  | LitBool of bool
  | LitUnit of unit
[@@deriving show]

let rec free (expr : t) : SymSet.t =
  let ( - ) set elt = SymSet.remove elt set in
  let ( + ) = SymSet.union in
  match expr with
  | Laz e -> free e
  | Let (name, e, body) -> free e + (free body - name)
  | Fix (binds, body) ->
      let binds = SymTab.of_list binds in
      let free_binds =
        SymTab.fold (fun _ t acc -> acc + free t) binds SymSet.empty
      in
      let free_binds =
        SymSet.filter (fun x -> not (SymTab.mem x binds)) free_binds
      in
      free_binds + free body
  | Cnd (test, then_, else_) -> free test + free then_ + free else_
  | Lam (param, body) -> free body - param
  | App (f, arg) -> free f + free arg
  | Var v -> SymSet.singleton v
  | LitInt _ | LitBool _ | LitUnit _ -> SymSet.empty
