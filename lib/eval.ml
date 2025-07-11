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
    | Control of (Env.t -> Ast.t -> k -> t)
  [@@deriving show]

  and k = t -> t

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
    | Control of (Env.t -> Ast.t -> k -> t)
  [@@deriving show]

  and k = t -> t

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

let rec eval (env : Env.t) (expr : Ast.t) (k : Value.k) : Value.t =
  let ( let* ) = ( @@ ) in
  match expr with
  | Laz e ->
      let env = Env.trim env e in
      k @@ Thunk (env, e, ref None)
  | Let (name, e, body) ->
      let* e = eval env e in
      let env = Env.extend env name e in
      eval env body k
  | Fix (binds, body) ->
      let names, exprs = List.split binds in
      let env = List.fold_left Env.knot env names in
      (* TODO: this will need attention when control ops are implemented *)
      let results = List.map (fun e -> eval env e Fun.id) exprs in
      let _ = List.map2 (Env.tie env) names results in
      eval env body k
  | Cnd (test, then_, else_) ->
      (let* (Bool cond) = eval env test in
       if cond then eval env then_ k else eval env else_ k)
      [@warning "-8"]
  | Lam (param, body) ->
      let env = Env.trim env body in
      k @@ Closure (env, param, body)
  | App (f, arg) -> (
      let* f = eval env f in
      match[@warning "-8"] f with
      | Closure (cl_env, param, body) ->
          let* actual = eval env arg in
          let cl_env = Env.extend cl_env param actual in
          eval cl_env body k
      | Builtin builtin ->
          let* actual = eval env arg in
          k @@ builtin actual
      | Control op -> op env arg k)
  | Var v -> k @@ Env.lookup env v
  | LitInt n -> k @@ Int n
  | LitBool b -> k @@ Bool b
  | LitUnit () -> k @@ Unit ()

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

  let call_cc =
    let ( let* ) = ( @@ ) in
    let call_cc env t cc =
      let* (Closure (cl_env, param, body)) = eval env t in
      let return env t _ = eval env t cc in
      let cl_env = Env.extend cl_env param (Control return) in
      eval cl_env body cc
    in
    Control call_cc
  in

  let force =
    Builtin
      (fun (Thunk (env, e, cell)) ->
        match !cell with
        | None ->
            eval env e (fun value ->
                cell := Some value;
                value)
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
      ("call/cc", call_cc);
    ]

let eval e = eval stdlib e Fun.id
let parse program = Parser.prog Lexer.read (Lexing.from_string program)
