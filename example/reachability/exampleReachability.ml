module type OpS = Fadbad.OpS


type time = float




module type StateS = sig
  module Op : OpS with type scalar = float
  type t

  val get: t -> int -> Op.t
  val map: (Op.t -> Op.t) -> t -> t

  val length: t -> int

  val to_array: t -> Op.t array
  val of_array: Op.t array -> t
end

module State (Op : OpS) = struct
  module Op = Op
  type t = Op.t array

  let of_array s = s
  let to_array a = a

  let get s i = s.(i)

  let map f s = Array.map f s

  let length s = Array.length s
end



module type FuncS =
  functor(State : StateS) ->
  sig
    val exec: State.t -> time -> State.t
  end

module ContinuousIntegrator
         (Func : FuncS)
         (Op : OpS with type scalar = float)
  =
  struct
    module T = Fadbad.T(Op)
    module StateOp = State(Op)
    module StateT = State(T)
    module Func = Func(StateT)

    let order = 3
    let dt = ref 0.01

    let state_op_to_t s =
      StateT.of_array (Array.map T.lift (StateOp.to_array s))

    let create_coef dim = Array.make dim (Op.zero ())

    let eval tm dim dt =
      let rec eval r l =
        match l with
        | [] -> r
        | h::t -> eval (Array.map2 (fun h r -> Op.(h + (scale r dt))) h r) t
      in
      eval (create_coef dim) tm

    let compute_tm s0 t0 =
      let dim = StateOp.length s0 in
      let s0 = state_op_to_t s0 in
      let deriv = Func.exec s0 t0 in
      let s0 = StateT.to_array s0 in
      let deriv = StateT.to_array deriv in
      let rec over_dim a o d =
        if d >= dim then
          a
        else
          let _ = T.eval deriv.(d) o in
          let () =
            T.set s0.(d) (o+1)
              (Op.scale (T.deriv deriv.(d) o) (1. /. (float (o+1))))
          in
          let () = Array.set a d (T.deriv s0.(d) (o+1)) in
          over_dim a o (d+1)
      in
      let rec over_order tm o =
        if o > order then
          List.rev tm
        else
          over_order ((over_dim (create_coef dim) o 0) :: tm) (o+1)
      in
      over_order [] 0

    (* TODO: add remainder computation *)
    let step s0 t0 =
      let dim = StateOp.length s0 in
      let tm = compute_tm s0 t0 in
      StateOp.of_array (eval tm dim !dt)

    let integrate s0 t0 t_end =
      let rec aux r s t =
        if t > t_end then
          List.rev (s :: r)
        else
          let next_s = step s t in
          let next_t = t +. !dt in
          aux (s :: r) next_s next_t
      in
      aux [] s0 t0
  end



module Brusselator (State : StateS with type Op.scalar = float) = struct
  let a = 1.5
  let b = 1.

  let exec s _ =
    let x = State.get s 0 in
    let y = State.get s 1 in
    let open State.Op in
    let xxy = x * x * y in
    let bx = scale x b in
    let dx = translate (xxy - bx - x) a in
    let dy = bx - xxy in
    State.of_array [| dx; dy |]
end


module type OpToPrint = sig
  include OpS with type scalar = float

  val print2d: t -> t -> unit
end

module Experiment (Func : FuncS) (Op : OpToPrint) = struct
  module Integrator = ContinuousIntegrator(Func)(Op)
  module State = State(Op)

  let print_state s dimX dimY =
    let x = State.get s dimX in
    let y = State.get s dimY in
    Op.print2d x y

  let rec print l =
    match l with
    | [] -> ()
    | h::t ->
       let () = print_state h 0 1 in
       let () = Printf.printf "\n" in
       print t

  let run s0 t0 t_end =
    let l = Integrator.integrate s0 t0 t_end in
    let () = print l in
    ()
end

let () =
  let module Op = Sets.AffineForm in
  let module State = State(Op) in
  let module Exp = Experiment (Brusselator) (Op) in
  let x0 = Op.make_bounds 1.9 2. in
  let y0 = Op.make_bounds 0. 0.1 in
  let s0 = State.of_array [| x0; y0 |] in
  let t0 = 0. in
  let t_end = 1. in
  let () = Exp.run s0 t0 t_end in
  ()
