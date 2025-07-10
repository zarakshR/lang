type symbol = string [@@deriving show]

type[@warning "-37"] term =
  | Laz of term
  | Let of symbol * term * term
  | Fix of (string * term) list * term
  | Cnd of term * term * term
  | Lam of symbol list * term
  | App of term * term list
  | Var of symbol
  | LitInt of int
  | LitBool of bool
  | LitUnit of unit
[@@deriving show]
