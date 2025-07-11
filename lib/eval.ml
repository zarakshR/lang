(* TODO: make `eval` CPSing
   let rec eval (env : env) (t : Ast.t) (k : value -> 'a) : 'a = *)
open Shared

module Value = struct
  type t =
    | Unit of unit
    | Bool of bool
    | Int of int
    | Pair of t * t
    | Cell of t ref
    | Thunk of env * Ast.t * t option ref
    | Closure of env * symbol * Ast.t
    | Builtin of (t -> t)
  [@@deriving show]

  and env = binding SymTab.t [@@deriving show]

  and binding =
    (* just a value *)
    | Plain of t
    (* a cell that will be filled in with a value later, for recursion *)
    | Cell of t option ref

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

  (* trim env w.r.t to `t` *)
  let trim env t = SymTab.filter (fun x _ -> SymSet.mem x (Ast.free t)) env
end

open Ast
open Value

let rec eval (env : env) (expr : Ast.t) : Value.t =
  let module ST = SymTab in
  match expr with
  | Laz e ->
      let env = trim env e in
      Thunk (env, e, ref None)
  | Let (name, e, body) ->
      let res = eval env e in
      let env = ST.add name (Plain res) env in
      eval env body
  | Fix (binds, body) ->
      let names, exprs = Seq.unzip (List.to_seq binds) in
      let names, exprs = (Seq.memoize names, Seq.memoize exprs) in
      let env =
        let extend env name = ST.add name (Cell (ref None)) env in
        Seq.fold_left extend env names
      in
      let results = Seq.map (eval env) exprs in
      let tie (name, result) =
        let[@warning "-8"] (Cell cell) = ST.find name env in
        cell := Some result
      in
      Seq.iter tie (Seq.zip names results);
      eval env body
  | Cnd (test, then_, else_) ->
      let[@warning "-8"] (Bool e) = eval env test in
      if e then eval env then_ else eval env else_
  | Lam (args, body) ->
      let env = trim env body in
      Closure (env, args, body)
  | App (f, arg) -> (
      let arg = eval env arg in
      match[@warning "-8"] eval env f with
      | Closure (env, param, body) ->
          let env = ST.add param (Plain arg) env in
          eval env body
      | Builtin builtin -> builtin arg)
  | Var v -> (
      try
        match ST.find v env with
        | Cell cell -> Option.get !cell
        | Plain value -> value
      with Invalid_argument _ | Not_found -> failwith ("not found: " ^ v))
  | LitInt n -> Int n
  | LitBool b -> Bool b
  | LitUnit () -> Unit ()

let[@warning "-8"] stdlib : env =
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
  SymTab.of_list
  @@ List.map
       (fun (sym, impl) -> (sym, Plain impl))
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
