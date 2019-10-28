module Brusselator (Op : Fadbad.OpS with type scalar = float) = struct
  let a = 1.
  let b = 1.7

  let exec s _ =
    let x = Array.get s 0 in
    let y = Array.get s 1 in
    let open Op in
    let xxy = x * x * y in
    let bx = scale x b in
    let dx = translate (xxy - bx - x) a in
    let dy = bx - xxy in
    [| dx; dy |]
end

module Experiment (Func : Fode.S) (Op : Sets.S) = struct
  module Integrator = ContinuousIntegrator.Make(Func)(Op)

  let to_interval x =
    let min, max = Op.get_min_max x in
    Interval.make_bounds min max

  let print_intervals s dimX dimY =
    let x = Array.get s dimX in
    let y = Array.get s dimY in
    Interval.print2d (to_interval x) (to_interval y)

  let print_state s dimX dimY =
    let x = Array.get s dimX in
    let y = Array.get s dimY in
    Op.print2d x y

  let print l =
    let rec print printer l =
      match l with
      | [] -> ()
      | h::t ->
         let () = printer h 0 1 in
         let () = Printf.printf "\n" in
         print printer t
    in
    print print_state l

  let run s0 t0 t_end =
    let () = Integrator.set_dt 1e-2 in
    let () = Integrator.set_order 3 in
    let l = Integrator.integrate s0 t0 t_end in
    let () = print l in
    ()
end


let () =
  let module Op = Sets.AffineForm in
  let module Exp = Experiment(Brusselator)(Op) in
  let s0 = [| Op.make_bounds 1.45 1.55; Op.make_bounds 2.95 3.05 |] in
  let t0 = 0. in
  let tEnd = 8. in
  let () = Exp.run s0 t0 tEnd in
  ()
