module OpFloat = Fadbad.OpFloat
module T = Fadbad.T(OpFloat)

let s = 10.
let r = 28.
let q = 8. /. 3.

type lorenz = {
  x : T.t; y : T.t; z : T.t; p : T.t; (* independent variables *)
  xp : T.t; yp : T.t; zp : T.t; pp : T.t; (* dependent variables *)
}

let create_lorenz x y z p =
  let open T in
  let s = make s in
  let r = make r in
  let q = make q in
  let x = make x in
  let y = make y in
  let z = make z in 
  let p = make p in
  let xp = p * (s * (y - x)) in
  let yp = p * (x * (r - z) - y) in
  let zp = p * ((x * y) - (q * z)) in
  let pp = make 0. in
  { x; y; z; p; xp; yp; zp; pp }

let reset_lorenz l = 
  let open T in
  reset l.xp; 
  reset l.yp; 
  reset l.zp; 
  reset l.pp

let _ = 
  (* construct ODE and set point of expansion *)
  let ode = create_lorenz 2.14736765 (-2.07804819) (r-.1.) 1.55865218 in

  for i = 0 to 9 do
    (* evaluate the i-th Taylor coefficient of the r.h.s of the ODE *)
    ignore(T.eval ode.xp i);
    ignore(T.eval ode.yp i);
    ignore(T.eval ode.zp i);
    ignore(T.eval ode.pp i);

    (* since d(x,y,z,p)/dt = lorenz(x,y,z,p) we have *)
    T.set ode.x (i+1) OpFloat.((T.deriv ode.xp i) / (integer Stdlib.(i+1)));
    T.set ode.y (i+1) OpFloat.((T.deriv ode.yp i) / (integer Stdlib.(i+1)));
    T.set ode.z (i+1) OpFloat.((T.deriv ode.zp i) / (integer Stdlib.(i+1)));
    T.set ode.p (i+1) OpFloat.((T.deriv ode.pp i) / (integer Stdlib.(i+1)));
  done;

  (* print out the Taylor coefficients for the solution of the ODE *)
  for i = 0 to 10 do
    Printf.printf "x[%d]=%f\n" i (T.d ode.x i);
    Printf.printf "y[%d]=%f\n" i (T.d ode.y i);
    Printf.printf "z[%d]=%f\n" i (T.d ode.z i);
    Printf.printf "p[%d]=%f\n" i (T.d ode.p i);
  done