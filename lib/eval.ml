(* TODO: make `eval` CPSing
   let rec eval (env : env) (t : Ast.t) (k : value -> 'a) : 'a = *)
open Shared

module rec Value : sig
  type t =
    | Unit of unit
    | Bool of bool
    | Int of int
    | Pair of t * t
    | Cell of t ref
    | Thunk of Env.t * Ast.t * t option ref
    | Closure of Env.t * symbol * Ast.t
    | Builtin of (t -> t)
  [@@deriving show]

  val equal : t -> t -> bool
end = struct
  type t =
    | Unit of unit
    | Bool of bool
    | Int of int
    | Pair of t * t
    | Cell of t ref
    | Thunk of Env.t * Ast.t * t option ref
    | Closure of Env.t * symbol * Ast.t
    | Builtin of (t -> t)
  [@@deriving show]

  let rec equal (x : t) (y : t) : bool =
    match (x, y) with
    | Unit (), Unit () -> true
    | Bool b1, Bool b2 -> b1 = b2
    | Int n1, Int n2 -> n1 = n2
    | Pair (x1, y1), Pair (x2, y2) -> equal x1 x2 && equal y1 y2
    | Thunk _, _ | _, Thunk _ -> raise (Invalid_argument "equality of thunk")
    | Closure _, _ | _, Closure _ ->
        raise (Invalid_argument "equality of closure")
    | Builtin _, _ | _, Builtin _ ->
        raise (Invalid_argument "equality of builtin")
    (* TODO: types? *)
    | _, _ -> false
end

and Env : sig
  type t [@@deriving show]

  val empty : t
  val lookup : t -> symbol -> Value.t
  val extend : t -> symbol -> Value.t -> t

  (* bind symbol to a knot, to be `tie`d later *)
  val knot : t -> symbol -> t

  (* tie a previously knotted symbol with a value *)
  val tie : t -> symbol -> Value.t -> unit

  (* trim env w.r.t to `expr` *)
  val trim : t -> Ast.t -> t
end = struct
  type binding =
    (* just a value *)
    | Plain of Value.t
    (* a cell that will be filled in with a value later, for self-reference
       (recursion, etc.) *)
    | Knot of Value.t option ref
  [@@deriving show]

  type t = binding SymTab.t [@@deriving show]

  let empty = SymTab.empty

  let lookup env name =
    try
      match SymTab.find name env with
      | Knot cell -> Option.get !cell
      | Plain value -> value
    with Invalid_argument _ | Not_found -> failwith ("lookup: " ^ name)

  let extend env name value = SymTab.add name (Plain value) env

  let knot env name =
    let cell : Value.t option ref = ref None in
    let env = SymTab.add name (Knot cell) env in
    env

  let tie env name value =
    let[@warning "-8"] (Knot cell) = SymTab.find name env in
    assert (!cell = None);
    let _ = cell := Some value in
    ()

  let trim env expr =
    SymTab.filter (fun x _ -> SymSet.mem x (Ast.free expr)) env
end

open Ast
open Value

let rec eval (env : Env.t) (expr : Ast.t) : Value.t =
  let module ST = SymTab in
  match expr with
  | Laz e ->
      let env = Env.trim env e in
      Thunk (env, e, ref None)
  | Let (name, e, body) ->
      let res = eval env e in
      let env = Env.extend env name res in
      eval env body
  | Fix (binds, body) ->
      let names, exprs = List.split binds in
      let env = List.fold_left Env.knot env names in
      let results = List.map (eval env) exprs in
      let _ = List.map2 (Env.tie env) names results in
      eval env body
  | Cnd (test, then_, else_) ->
      let[@warning "-8"] (Bool e) = eval env test in
      if e then eval env then_ else eval env else_
  | Lam (args, body) ->
      let env = Env.trim env body in
      Closure (env, args, body)
  | App (f, arg) -> (
      let actual = eval env arg in
      match[@warning "-8"] eval env f with
      | Closure (env, param, body) ->
          let env = Env.extend env param actual in
          eval env body
      | Builtin builtin -> builtin actual)
  | Var v -> Env.lookup env v
  | LitInt n -> Int n
  | LitBool b -> Bool b
  | LitUnit () -> Unit ()

let[@warning "-8"] stdlib : Env.t =
  let add = Builtin (fun (Int x) -> Builtin (fun (Int y) -> Int (x + y))) in
  let sub = Builtin (fun (Int x) -> Builtin (fun (Int y) -> Int (x - y))) in
  let mul = Builtin (fun (Int x) -> Builtin (fun (Int y) -> Int (x * y))) in
  let eq = Builtin (fun x -> Builtin (fun y -> Bool (Value.equal x y))) in
  let cons = Builtin (fun x -> Builtin (fun y -> Pair (x, y))) in

  let car = Builtin (fun (Pair (x, _)) -> x) in
  let cdr = Builtin (fun (Pair (_, x)) -> x) in

  let ref = Builtin (fun x -> Cell (ref x)) in
  let ref_set =
    Builtin (fun (Cell cell) -> Builtin (fun x -> Unit (cell := x)))
  in
  let ref_get = Builtin (fun (Cell cell) -> !cell) in

  let print_int = Builtin (fun (Int n) -> Unit (Format.printf "%d" n)) in
  let print_bool = Builtin (fun (Bool b) -> Unit (Format.printf "%B" b)) in
  let print_unit = Builtin (fun (Unit ()) -> Unit (Format.printf "()")) in
  let print_nl = Builtin (fun (Unit ()) -> Unit (Format.printf "\n")) in

  let force =
    Builtin
      (fun (Thunk (env, e, cell)) ->
        match !cell with
        | None ->
            let value = eval env e in
            cell := Some value;
            value
        | Some value -> value)
  in
  List.fold_left
    (fun env (name, binding) -> Env.extend env name binding)
    Env.empty
    [
      ("+", add);
      ("-", sub);
      ("*", mul);
      ("=?", eq);
      ("cons", cons);
      ("car", car);
      ("cdr", cdr);
      ("print_int", print_int);
      ("print_bool", print_bool);
      ("print_unit", print_unit);
      ("print_nl", print_nl);
      ("force", force);
      ("ref", ref);
      (":=", ref_set);
      ("!", ref_get);
    ]

let eval = eval stdlib
let parse program = Parser.prog Lexer.read (Lexing.from_string program)
