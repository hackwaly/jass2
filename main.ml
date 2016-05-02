exception Exit

let main () =
  let ctx = Eval.create () in
  Eval.load ctx stdin;
  let value = Eval.call ctx "main" [] in
  Printf.printf "%s\n" (Eval.s_value value)
;;

main ()
