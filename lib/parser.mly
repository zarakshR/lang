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
%start<term> prog

%%

let prog := ~ = term; EOF; <>

let term :=
  | LPAREN; ~ = term; RPAREN; <>
  (* application *)
  | LPAREN; func = term; args = term+; RPAREN; <App>
  | LAZY; ~ = term; <Laz>
  | LET; (name, expr) = binding; IN; ~ = term; <Let>
  | FIX; ~ = separated_nonempty_list(AND, binding); IN; ~ = term; <Fix>
  | IF; test = term; THEN; then_ = term; ELSE; else_ = term; <Cnd>
  | LAMBDA; ~ = SYMBOL+; ARROW; ~ = term; <Lam>
  | ~ = SYMBOL; <Var>
  | ~ = INT; <LitInt>
  | ~ = BOOL; <LitBool>
  | ~ = UNIT; <LitUnit>

let binding :=
  symbols = SYMBOL+; EQUALS; ~ = term; {
    match symbols with
    | [] -> assert false
    | name :: [] -> (name, term)
    | name :: args -> (name, Lam (args, term))
  }
