let () =
  if Array.length Sys.argv <> 2 then (
    Format.fprintf Format.err_formatter "usage: %s <filename>" Sys.argv.(0);
    exit 1)
  else
    let eval in_ch = Interpreter.Eval.(eval @@ parse @@ In_channel.input_all in_ch) in
    let result = In_channel.with_open_text Sys.argv.(1) eval in
    Interpreter.Eval.Value.pp Format.std_formatter result
