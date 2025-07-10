module I = Interpreter
module Value = I.Eval.Value

let parse program = I.Parser.prog I.Lexer.read (Lexing.from_string program)
let eval = I.Eval.eval

(* TODO *)
let test_parser = []

let test_eval =
  let test_simple_app () =
    let program = {|((\x -> x) 1)|} in
    let expected = Value.Int 1 in
    Alcotest.check
      (module Value)
      "simple application" expected
      (eval (parse program))
  in

  let test_multiple_app () =
    let program = {|
    ((\x y z -> (+ (+ x y) z)) 1 1 1)
|} in
    let expected = Value.Int 3 in
    Alcotest.check
      (module Value)
      "simple application" expected
      (eval (parse program))
  in

  let test_let_binding () =
    let program = {|
    let x = (\x -> (+ x 2)) in (+ (x 2) (x 3))
|} in
    let expected = Value.Int 9 in
    Alcotest.check (module Value) "let binding" expected (eval (parse program))
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
    Alcotest.check (module Value) "recursion" expected (eval (parse program))
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
    Alcotest.check (module Value) "shadowing" expected (eval (parse program))
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
    Alcotest.check
      (module Value)
      "pair primitives" expected
      (eval (parse program))
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
    Alcotest.check
      (module Value)
      "mutual recursion" expected
      (eval (parse program))
  in

  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ("simple application", test_simple_app);
      ("multiple application", test_multiple_app);
      ("let binding", test_let_binding);
      ("recursion", test_recursion);
      ("shadowing", test_shadowing);
      ("pair primitives", test_pair_primitives);
      ("mutual recursion", test_mutual_recursion);
    ]

let test_sample_programs =
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
    let expected =
      let fibs =
        List.map (fun n -> Value.Int n) [ 0; 1; 1; 2; 3; 5; 8; 13; 21; 34 ]
      in
      List.fold_right (fun n acc -> Value.Pair (n, acc)) fibs (Unit ())
    in
    Alcotest.check (module Value) "fibonacci" expected (eval (parse program))
  in

  [ Alcotest.test_case "fibonacci" `Slow test_fibonacci ]

let () =
  Alcotest.run "Interpreter"
    [
      ("parser", test_parser);
      ("eval", test_eval);
      ("sample programs", test_sample_programs);
    ]
