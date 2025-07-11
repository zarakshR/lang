type symbol = string [@@deriving show]

type term =
  | Laz of term
  | Let of symbol * term * term
  | Fix of (string * term) list * term
  | Cnd of term * term * term
  | Lam of symbol * term
  | App of term * term
  | Var of symbol
  | LitInt of int
  | LitBool of bool
  | LitUnit of unit
[@@deriving show]
