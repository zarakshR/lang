%{ open Ast %}

(* Tokens *)
(* literals *)
%token<int>  INT
%token<unit> UNIT
%token<bool> BOOL
%token<string> SYMBOL

(* keywords *)
%token LPAREN
%token RPAREN
%token LAZY
%token LET
%token EQUALS
%token IN
%token FIX
%token AND
%token IF
%token THEN
%token ELSE
%token LAMBDA
%token ARROW

(* eof *)
%token EOF

(* Grammar *)
%start<Ast.t> prog

%%

let prog := ~ = expr; EOF; <>

let expr :=
  | LPAREN; ~ = expr; RPAREN; <>
  (* application *)
  | LPAREN; func = expr; args = expr+; RPAREN; {
    List.fold_left (fun acc arg -> App (acc, arg)) func args
  }
  | LAZY; ~ = expr; <Laz>
  | LET; (name, expr) = binding; IN; body = expr; <Let>
  | FIX; ~ = separated_nonempty_list(AND, binding); IN; ~ = expr; <Fix>
  | IF; test = expr; THEN; then_ = expr; ELSE; else_ = expr; <Cnd>
  | LAMBDA; params = SYMBOL+; ARROW; ~ = expr; {
    List.fold_right (fun param acc -> Lam (param, acc)) params expr
  }
  | ~ = SYMBOL; <Var>
  | ~ = INT; <LitInt>
  | ~ = BOOL; <LitBool>
  | ~ = UNIT; <LitUnit>

let binding :=
  symbols = SYMBOL+; EQUALS; ~ = expr; {
    let [@warning "-8"] name :: params = symbols in
    let expr =
      List.fold_right (fun param acc -> Lam (param, acc)) params expr
    in
    (name, expr)
  }
