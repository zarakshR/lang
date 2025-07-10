(* TODO: distinguish mutable (option ref) and plain values in envs *)
(* TODO: add mutable ref-cells *)
(* TODO: make `eval` CPSing
   let rec eval (env : env) (t : term) (k : value -> 'a) : 'a = *)
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

module Value = struct
  type t =
    | Unit of unit
    | Bool of bool
    | Int of int
    | Pair of t * t
    | Thunk of env * Ast.term * t option ref
    | Closure of env * Ast.symbol list * Ast.term
    | Builtin of int * (t list -> t)
  [@@deriving show]

  and env = t option ref SymTab.t [@@deriving show]

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

open Ast
open Value

let rec free (t : term) : SymSet.t =
  let ( - ) set elt = SymSet.remove elt set in
  let ( + ) = SymSet.union in
  match t with
  | Laz t -> free t
  | Let (x, t, body) -> free t + (free body - x)
  | Rec (x, t, body) -> free t - x + (free body - x)
  | Fix (binds, body) ->
      let binds = SymTab.of_list binds in
      let free_binds =
        SymTab.fold (fun _ t acc -> acc + free t) binds SymSet.empty
      in
      let free_binds =
        SymSet.filter (fun x -> not (SymTab.mem x binds)) free_binds
      in
      free_binds + free body
  | Cnd (t, then_, else_) -> free t + free then_ + free else_
  | Lam (args, body) ->
      List.fold_left (fun acc arg -> acc - arg) (free body) args
  | App (f, args) ->
      let free_args =
        List.fold_left (fun acc arg -> acc + free arg) SymSet.empty args
      in
      free f + free_args
  | Var v -> SymSet.singleton v
  | LitInt _ | LitBool _ | LitUnit _ -> SymSet.empty

let rec eval (env : env) (t : term) : Value.t =
  let module ST = SymTab in
  (* trim env w.r.t to `t` *)
  let trim t = ST.filter (fun x _ -> SymSet.mem x (free t)) env in

  match t with
  | Laz t ->
      let env = trim t in
      Thunk (env, t, ref None)
  | Let (x, t, body) ->
      let e = eval env t in
      let env = ST.add x (ref (Some e)) env in
      eval env body
  | Rec (x, t, body) ->
      let cell = ref None in
      let env = ST.add x cell env in
      let e = eval env t in
      cell := Some e;
      eval env body
  | Fix (binds, body) ->
      let binds = SymTab.of_list binds in
      (* map each name to a mutable cell, each initially empty *)
      let cells = ST.mapi (fun _ _ -> ref None) binds in
      (* bind each name to its cell in env *)
      let env = ST.union (fun _ _ e -> Some e) env cells in
      (* evaluate each binding with this env, mapping names to results *)
      let results = ST.map (fun t -> eval env t) binds in
      (* tie all the knots *)
      let tie name = ST.find name cells := Some (ST.find name results) in
      ST.iter (fun x _ -> tie x) binds;
      (* assert nothing left untied *)
      assert (ST.for_all (fun _ cell -> Option.is_some !cell) env);
      eval env body
  | Cnd (t, then_, else_) ->
      let[@warning "-8"] (Bool e) = eval env t in
      if e then eval env then_ else eval env else_
  | Lam (args, body) ->
      let env = trim body in
      Closure (env, args, body)
  | App (f, args) -> (
      let args = List.map (fun arg -> eval env arg) args in
      match[@warning "-8"] eval env f with
      | Closure (closure_env, params, body) ->
          if List.length args <> List.length params then
            failwith "arity mismatch";
          let actuals =
            let bind param arg = (param, ref (Some arg)) in
            List.map2 bind params args
          in
          let closure_env =
            ST.union (fun _ _ x -> Some x) closure_env (ST.of_list actuals)
          in
          eval closure_env body
      | Builtin (arity, builtin) ->
          if arity <> List.length args then failwith "arity mismatch";
          builtin args)
  | Var v -> (
      try Option.get !(ST.find v env)
      with Invalid_argument _ | Not_found -> failwith ("not found: " ^ v))
  | LitInt n -> Int n
  | LitBool b -> Bool b
  | LitUnit () -> Unit ()

let[@warning "-8"] stdlib : env =
  let binary f = Builtin (2, f) in
  let unary f = Builtin (1, f) in

  let add = binary (fun [ Int x; Int y ] -> Int (x + y)) in
  let sub = binary (fun [ Int x; Int y ] -> Int (x - y)) in
  let mul = binary (fun [ Int x; Int y ] -> Int (x * y)) in
  let eq = binary (fun [ x; y ] -> Bool (Value.equal x y)) in
  let cons = binary (fun [ x; y ] -> Pair (x, y)) in

  let car = unary (fun [ Pair (x, _) ] -> x) in
  let cdr = unary (fun [ Pair (_, x) ] -> x) in

  let print_int = unary (fun [ Int n ] -> Unit (Format.printf "%d" n)) in
  let print_bool = unary (fun [ Bool b ] -> Unit (Format.printf "%B" b)) in
  let print_unit = unary (fun [ Unit () ] -> Unit (Format.printf "()")) in
  let print_nl = unary (fun [ Unit () ] -> Unit (Format.printf "\n")) in

  let force =
    unary (fun [ Thunk (env, term, cell) ] ->
        match !cell with
        | None ->
            let value = eval env term in
            cell := Some value;
            value
        | Some value -> value)
  in
  SymTab.of_list
  @@ List.map
       (fun (sym, impl) -> (sym, ref (Some impl)))
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
       ]

let eval = eval stdlib
let parse program = Parser.prog Lexer.read (Lexing.from_string program)

(* "21"; 15 *)
let _t8 =
  let prog =
    {|
    let n = lazy (let _ = (print_int 1) in 5) in
    let _ = (print_int 2) in
    (+ (+ (force n) (force n)) (force n))
  |}
  in
  parse prog
