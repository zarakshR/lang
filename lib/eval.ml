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
    | Cell of t ref
    | Thunk of env * Ast.term * t option ref
    | Closure of env * Ast.symbol * Ast.term
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
end

open Ast
open Value

let rec free (t : term) : SymSet.t =
  let ( - ) set elt = SymSet.remove elt set in
  let ( + ) = SymSet.union in
  match t with
  | Laz t -> free t
  | Let (x, t, body) -> free t + (free body - x)
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
  | Lam (param, body) -> free body - param
  | App (f, arg) -> free f + free arg
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
      let env = ST.add x (Plain e) env in
      eval env body
  | Fix (binds, body) ->
      let binds = SymTab.of_list binds in
      let cells = ST.mapi (fun _ _ -> Cell (ref None)) binds in
      let env = ST.union (fun _ _ c -> Some c) env cells in
      let results = ST.map (fun t -> eval env t) binds in
      let tie name =
        match ST.find name cells with
        | Cell cell -> cell := Some (ST.find name results)
        | Plain _ -> ()
      in
      ST.iter (fun x _ -> tie x) binds;
      (* assert no knot left untied *)
      let () =
        let is_tied = function
          | Cell cell -> Option.is_some !cell
          | Plain _ -> true
        in
        assert (ST.for_all (fun _ -> is_tied) cells)
      in
      eval env body
  | Cnd (t, then_, else_) ->
      let[@warning "-8"] (Bool e) = eval env t in
      if e then eval env then_ else eval env else_
  | Lam (args, body) ->
      let env = trim body in
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
      (fun (Thunk (env, term, cell)) ->
        match !cell with
        | None ->
            let value = eval env term in
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
