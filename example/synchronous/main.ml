module Op = Fadbad.F(Fadbad.OpFloat)

let step (Ztypes.Node { alloc; reset; step }) =
  let mem = alloc () in
  reset mem;
  step mem

let _ =
  let step = step Func.aut in

  let x0 = Op.make 5. in
  Op.diff x0 0 1;

  let step_no = ref 0 in
  while true do
    let y = step x0 in
    Printf.printf "step: %d\n" !step_no;
    Printf.printf "x0 = %f\n" (Op.get x0);
    Printf.printf "y_%d = %f\n" !step_no (Op.get y);
    Printf.printf "dy_%d/dx0 = %f\n" !step_no (Op.d y 0);
    flush stdout;
    ignore (input_line stdin);
    step_no := !step_no + 1;
  done
