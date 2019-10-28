open Common

type tad_values = {
  tad_t : float;
  tad_x : float;
  tad_y : float;
  tad_dxdt : float array;
  tad_dydt : float array;
}

let print_tad_values ff values =
Format.fprintf ff "@[<v 2>{@;%a,@;%a,@;%a,@;%a,@;%a@]@;}"
  (print_float "t") values.tad_t
  (print_float "x") values.tad_x
  (print_float "y") values.tad_y
  (print_float_array "dx/dt") values.tad_dxdt
  (print_float_array "dy/dt") values.tad_dydt

let print_tad_res = print_res print_tad_values

module TOp = Fadbad.T(Fadbad.OpFloat)

let main_tad nsteps dt ncoeff =
  let open Brusselator.Make(TOp) in
  let v0 = { x = TOp.one (); y = TOp.one () } in
  let { alloc; step; reset } = make_euler v0 brusselator in
  let t = ref 0. in

  let mem = alloc () in reset mem;
  let exec_t = Unix.gettimeofday () in
  ignore (step mem dt); (* initialization *)
  (* start loop *)
  for i = 1 to nsteps do
    ignore (step mem dt);
    t := !t +. dt;
  done;
  (* end loop *)

  TOp.set v0.x 1 (Fadbad.OpFloat.one ());
  TOp.set v0.y 1 (Fadbad.OpFloat.one ());

  ignore (TOp.eval mem.lastv.x ncoeff);
  ignore (TOp.eval mem.lastv.y ncoeff);
  let cur_exec_time = Unix.gettimeofday () -. exec_t in
  {
    exec_time = cur_exec_time;
    dt; nsteps;
    values = {
      tad_t = !t;
      tad_x = TOp.get mem.lastv.x;
      tad_y = TOp.get mem.lastv.y;
      tad_dxdt = TOp.get_derivatives mem.lastv.x;
      tad_dydt = TOp.get_derivatives mem.lastv.y;
    }
  }

let _ =
  let nsteps = ref default_nsteps in
  let dt = ref default_dt in
  let ncoeff = ref default_ncoeff in

  Arg.(parse [
    "-n", Set_int nsteps,
      Printf.sprintf "number of steps to compute (default: %d)" !nsteps;
    "-dt", Set_float dt,
      Printf.sprintf "size of one step (default: %f)" !dt;
    "-ncoeff", Set_int ncoeff,
      Printf.sprintf "number of taylor coefficients to compute (default: %d)"
                      !ncoeff;

  ]) (fun s -> ()) "usage: ./tad_ml [[-]-help] [-n N] [-dt DT]\n";

  let res = main_tad !nsteps !dt !ncoeff in
  Format.printf "%a@." print_tad_res res
