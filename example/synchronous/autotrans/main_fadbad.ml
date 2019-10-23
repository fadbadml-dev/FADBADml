let step (Ztypes.Cnode { alloc; reset; copy; step }) =
  let mem = alloc () in
  reset mem;
  step mem

let _ =
  let step = step Autotransd.autotrans in

  let tstep = MyOp.make Autotransd.timestep in

  let t = ref (MyOp.make 0.) in
  let throttle = MyOp.make 84.0135993263 in
  let brake = MyOp.make 279.482038615 in

  let step_no = ref 0 in
  while true do
    (* let (t, throttle, brake, rpm, gear, speed) = step () in *)
    MyOp.diff !t 0 3;
    MyOp.diff throttle 1 3;
    MyOp.diff brake 2 3;
    let (rpm, gear, speed) = step (!t, (throttle, brake)) in
    Printf.printf "----------- step: %d\n" !step_no;
    print_newline ();
    Printf.printf "\tt = %f\n" (MyOp.get !t);
    Printf.printf "\tthrottle = %f\n" (MyOp.get throttle);
    Printf.printf "\tbrake = %f\n" (MyOp.get brake);
    print_newline ();
    Printf.printf "\trpm = %f\n" (MyOp.get rpm);
    Printf.printf "\tgear = %f\n" (MyOp.get gear);
    Printf.printf "\tspeed = %f\n" (MyOp.get speed);
    print_newline ();
    Printf.printf "\tdrpm/dt = %f\n" (MyOp.d rpm 0);
    Printf.printf "\tdgear/dt = %f\n" (MyOp.d gear 0);
    Printf.printf "\tdspeed/dt = %f\n" (MyOp.d speed 0);
    print_newline ();
    Printf.printf "\tdrpm/dthrottle = %f\n" (MyOp.d rpm 1);
    Printf.printf "\tdgear/dthrottle = %f\n" (MyOp.d gear 1);
    Printf.printf "\tdspeed/dthrottle = %f\n" (MyOp.d speed 1);
    print_newline ();
    Printf.printf "\tdrpm/dbrake = %f\n" (MyOp.d rpm 2);
    Printf.printf "\tdgear/dbrake = %f\n" (MyOp.d gear 2);
    Printf.printf "\tdspeed/dbrake = %f\n" (MyOp.d speed 2);
    Printf.printf "-----------\n";
    flush stdout;
    ignore (input_line stdin);
    step_no := !step_no + 1;
    t := MyOp.(!t + tstep)
  done
