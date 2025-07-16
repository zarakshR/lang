module I = Interpreter
module Value = I.Eval.Value

let parse program = I.Parser.prog I.Lexer.read (Lexing.from_string program)
let eval = I.Eval.eval
let check msg expected value = Alcotest.check (module Value) msg expected value

(* TODO: rewrite these so that eval tests are not dependent on parsing *)
let test_parser = []

let test_eval =
  let test_simple_app () =
    let program = {|((\x -> x) 1)|} in
    let expected = Value.Int 1 in
    check "simple application" expected (eval (parse program))
  in

  let test_multiple_app () =
    let program = {|
    ((\x y z -> (+ (+ x y) z)) 1 1 1)
|} in
    let expected = Value.Int 3 in
    check "simple application" expected (eval (parse program))
  in

  let test_let_binding () =
    let program = {|
    let x = (\x -> (+ x 2)) in (+ (x 2) (x 3))
|} in
    let expected = Value.Int 9 in
    check "let binding" expected (eval (parse program))
  in

  let test_recursion () =
    let program =
      {|
    fix fact x =
      if (=? x 1) then 1 else (* x (fact (- x 1)))
    in
    (fact 4)
|}
    in
    let expected = Value.Int 24 in
    check "recursion" expected (eval (parse program))
  in

  let test_shadowing () =
    let program =
      {|
    let x = 1 in
    let closure = (let x = 2 in (\y -> (cons x y))) in
    (closure x)
|}
    in
    let expected = Value.(Pair (Int 2, Int 1)) in
    check "shadowing" expected (eval (parse program))
  in

  let test_pair_primitives () =
    let program =
      {|
    let p = (cons 1 2) in
    let flip p = (cons (cdr p) (car p)) in
    (cons (flip p) (flip (flip p)))
|}
    in
    let expected = Value.(Pair (Pair (Int 2, Int 1), Pair (Int 1, Int 2))) in
    check "pair primitives" expected (eval (parse program))
  in

  let test_currying () =
    let program =
      {|
    let f x y z = (+ (+ x y) z) in
    let a = (f 1) in
    let b = (f 1 2) in
    let c = (f 1 2 3) in
    (cons (cons (a 2 3) (b 3)) c)
|}
    in
    let expected = Value.(Pair (Pair (Int 6, Int 6), Int 6)) in
    check "currying" expected (eval (parse program))
  in

  let test_mutual_recursion () =
    let program =
      {|
    fix is_even n = if (=? n 0) then true else (is_odd (- n 1))
    and is_odd n = if (=? n 0) then false else (is_even (- n 1)) in
    let trues = (cons (is_even 2) (is_odd 1)) in
    let falses = (cons (is_odd 6) (is_even 7)) in
    (cons trues falses)
|}
    in
    let expected =
      Value.(Pair (Pair (Bool true, Bool true), Pair (Bool false, Bool false)))
    in
    check "mutual recursion" expected (eval (parse program))
  in

  let test_ref_cells () =
    let program =
      {|
    let cell = (ref 1) in
    let old = (! cell) in
    let _ = (:= cell 2) in
    let new = (! cell) in
    (cons old new)
|}
    in
    let expected = Value.(Pair (Int 1, Int 2)) in
    check "ref cells" expected (eval (parse program))
  in

  let test_laziness () =
    let program =
      {|
    let cell = (ref 0) in
    let incr =
      let n = (! cell) in
      (:= cell (+ n 1))
    in
    let n = lazy (let _ = (incr) in 5) in
    let n = (+ (force n) (force n)) in
    (cons (! cell) n)
|}
    in
    let expected = Value.(Pair (Int 1, Int 10)) in
    check "laziness" expected (eval (parse program))
  in

  let test_call_cc () =
    let program =
      {|
    let x = (+ (call/cc (\k -> (k 1))) 1) in
    let y = (call/cc (\k -> (+ 1 (k 1)))) in
    (cons x y)
|}
    in
    let expected = Value.(Pair (Int 2, Int 1)) in
    check "call/cc" expected (eval (parse program))
  in

  let test_shift_reset () =
    let program =
      {|
    let x = (reset (\_ -> (
              (- (+ 3 (shift (\k -> (* 5 2)))) 1))))
    in
    let y = (- (reset (\_ -> (+ 3 (shift (\k -> (* 5 2)))))) 1)
    in
    (cons x y)
|}
    in
    let expected = Value.(Pair (Int 10, Int 9)) in
    check "shift/reset" expected (eval (parse program))
  in

  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ("simple application", test_simple_app);
      ("multiple application", test_multiple_app);
      ("let binding", test_let_binding);
      ("currying", test_currying);
      ("recursion", test_recursion);
      ("shadowing", test_shadowing);
      ("pair primitives", test_pair_primitives);
      ("mutual recursion", test_mutual_recursion);
      ("ref cells", test_ref_cells);
      ("laziness", test_laziness);
      ("call/cc", test_call_cc);
      ("shift/reset", test_shift_reset);
    ]

let test_sample_programs =
  (* build an object language list of Ints from a list of OCaml ints *)
  let value_list_of_ints ints =
    List.fold_right
      (fun n value_list -> Value.(Pair (Int n, value_list)))
      ints (Unit ())
  in
  let value_list_of_pairs pairs =
    List.fold_right
      (fun (x, y) value_list -> Value.(Pair (Pair (x, y), value_list)))
      pairs (Unit ())
  in

  let test_fibonacci () =
    let program =
      {|
    fix stream_drop n s =
      if (=? n 0) then s else
        (stream_drop (- n 1) (force (cdr s)))
    in
    fix stream_take n s =
      if (=? n 0) then () else
        let head = (force (car s)) in
        let tail = (force (cdr s)) in
        (cons head (stream_take (- n 1) tail))
    in
    fix stream_zip_with f s1 s2 =
      let a = (car s1) in
      let a_tail = (cdr s1) in
      let b = (car s2) in
      let b_tail = (cdr s2) in
      let head = lazy (f (force a) (force b)) in
      let tail = lazy (stream_zip_with f (force a_tail) (force b_tail)) in
      (cons head tail)
    in
    fix fibs =
      let tail = lazy (stream_zip_with + fibs (stream_drop 1 fibs)) in
      (cons (lazy 0) (lazy (cons (lazy 1) tail)))
    in
    (stream_take 10 fibs)
|}
    in
    let expected = value_list_of_ints [ 0; 1; 1; 2; 3; 5; 8; 13; 21; 34 ] in
    check "fibonacci" expected (eval (parse program))
  in

  let test_imperative () =
    let program =
      {|
    let make_label _ = (call/cc (\k -> k)) in
    let goto label = (label label) in
    let gen_range source from to =
      (call/cc (\return ->
        let counter = (ref to) in
        let decr _ = (:= counter (- (! counter) 1)) in
        let l = (ref ()) in
        let loop_label = (make_label ()) in
        if (=? (! counter) from) then
          (return (! l))
        else
          let _ = (decr ()) in
          let _ = (:= l (cons (source (! counter)) (! l))) in
          (goto loop_label)))
    in
    let list_search n l =
      (call/cc (\return ->
        fix list_search l idx =
          if (=? (car l) n) then
            (return idx)
          else
            (+ idx (list_search (cdr l) (+ idx 1)))
        in
        (list_search l 0)))
    in
    let gen_serial = (gen_range (\x -> x)) in
    let gen_squares = (gen_range (\x -> (* x x))) in
    let a = (list_search 3 (gen_serial 0 10)) in
    let b = (list_search 25 (gen_squares 0 10)) in
    (cons a b)
  |}
    in
    let expected = Value.(Pair (Int 3, Int 5)) in
    check "imperative" expected (eval (parse program))
  in

  let test_ping_pong () =
    let program =
      {|
      let yield x = (shift (\k -> (cons x k))) in
      let ping n =
        (reset (\_ ->
          fix ping n =
            let ret = (yield (+ n 1)) in
            (ping ret)
          in
          (ping n)))
      in
      let pong n =
        (reset (\_ ->
          fix pong n =
            let ret = (yield (- n 1)) in
            (pong ret)
          in
          (pong n)))
      in
      fix driver ping pong num_turns =
        if (=? num_turns 0) then
          ()
        else
          let n = (car ping) in
          let m = (car pong) in
          let ping = (cdr ping) in
          let pong = (cdr pong) in
          let next = (driver (ping m) (pong n) (- num_turns 1)) in
          (cons n (cons m next))
      in
      (driver (ping 0) (pong 1) 3)
  |}
    in
    let expected = value_list_of_ints [ 1; 0; 1; 0; 1; 0 ] in
    check "ping-pong" expected (eval (parse program))
  in

  let test_amb () =
    let program =
      {|
      let l = (ref ()) in
      let prepend x = (:= l (cons x (! l))) in
      let amb choices =
        (shift (\k ->
          fix amb choices =
            if (=? choices ()) then
              ()
            else
              let _ = (k (car choices)) in
              (amb (cdr choices))
          in
          (amb choices)))
      in
      let ambguard x =
        (shift (\k ->
          if (=? x ()) then
            ()
          else if x then
            (k x)
          else
            ()))
      in
      -- non-deterministically add to l, all (x,y) s.t x = y from (0 1 2)
      let _ =
        (reset (\_ ->
          let x = (amb (cons 0 (cons 1 (cons 2 ())))) in
          let y = (amb (cons 0 (cons 1 (cons 2 ())))) in
          let _ = (ambguard (=? x y)) in
          (prepend (cons x y))))
      in
      (! l)

  |}
    in
    let expected =
      value_list_of_pairs
      @@ List.combine
           (List.map (fun n -> Value.Int n) [ 2; 1; 0 ])
           (List.map (fun n -> Value.Int n) [ 2; 1; 0 ])
    in
    check "amb" expected (eval (parse program))
  in

  [
    Alcotest.test_case "fibonacci" `Slow test_fibonacci;
    Alcotest.test_case "imperative" `Slow test_imperative;
    Alcotest.test_case "ping-pong" `Slow test_ping_pong;
    Alcotest.test_case "amb" `Slow test_amb;
  ]

let () =
  Alcotest.run "Interpreter"
    [
      ("parser", test_parser);
      ("eval", test_eval);
      ("sample programs", test_sample_programs);
    ]
