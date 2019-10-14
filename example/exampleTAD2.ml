module OpFloat = Fadbad.OpFloat
module T = Fadbad.T(OpFloat)

type tode = {
  x : T.t; (* independent variable *)
  xp : T.t; (* dependent variable *)
}

let create_tode x = 
  let x = T.make x in
  let xp = T.cos x in (* record DAG at construction *)
  { x; xp }

let _ = 
  let ode = create_tode 1. in (* construct ode and set point of expansion *)
  
  for i = 0 to 9 do
    ignore(T.eval ode.xp i); (* evaluate the i-th Taylor coefficient *)

    (* use dx/dt=ode(x)*)
    T.set ode.x (i+1) OpFloat.((T.deriv ode.xp i) / (integer Stdlib.(i+1)))
  done;

  (* T.d ode.x i now contains the i-th Taylor coefficient of 
     the solution of the ODE *)

  (* Print out the Taylor coefficients for the solution of the ODE *)

  for i = 0 to 10 do
    Printf.printf "x[%d]=%f\n" i (T.d ode.x i);
  done
