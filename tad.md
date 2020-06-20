[Fadbad.T]: doc/Fadbad.T.html
[FADBAD++ website]: http://www.fadbad.com/fadbad.html#CrashCourse

## Automatic Taylor expansion (TAD)

See [Fadbad.T]

In Taylor expansion mode, as in backward mode, FADBADml constructs a graph of the expressions while computing their values. Then, as in forward mode, it propagate coefficients of the Taylor expansion from the sink nodes to the roots of the graph.

The type `T(Op).t` is the following record:

~~~ocaml
type t = {
  mutable operator : op;
  mutable operands : t array;
  mutable tvalues : OpTValues.t;
}
~~~

with `OpTValues.t` which stores the coefficients:

~~~ocaml
type t = {
  mutable n : int;
  values : T.t array;
}
~~~

A node stores its operator, its operands and the values of the coefficients of Taylor expansion of its sub-tree.

For example, the function `f(x,y) = y * sqrt(x) + sin(sqrt(x))` would be implemented as

~~~ocaml
let f x y =
  let open T in
  let z = sqrt x in
  y * z + (sin z)
~~~

and it would be used like this (with `module T = Fadbad.T(OpFloat)`):

~~~ocaml
...
let x = T.make 1. in (* Initialize variable x *)
let y = T.make 2. in (* Initialize variable y *)
T.set x 1 (OpFloat.one ()); (* Taylor-expand wrt. x (dx/dx=1) *)
let res = f x y in
...
~~~

To compute the Taylor coefficients, we have to call to function `T.eval`. To compute an expansion of the previous example to the order 10, we have to execute:
~~~ocaml
...
ignore (T.eval res 10);
...
~~~
The computed coefficients are stored in the variable `res`. We can access to the value of `res` with `T.get res` and to the i-th coefficient of the expansion with `T.d res i` (with $0 \leq i \leq 10$ because of expansion's order).
Find the whole code [here](https://github.com/fadbadml-dev/FADBADml/blob/master/example/exampleTAD1.ml).

### Ordinary differential equation (ODE)

This module can also be used to compute a Taylor expansion of the solution of a given ordinary differential equation.
We can define a record to encode the ODE via the input and the corresponding derivative:
~~~ocaml
type tode = {
  x : T.t; (* independent variable *)
  xp : T.t; (* dependent variable *)
}
~~~
and produce the desired ODE, here `x' = cos(x)`:
~~~ocaml
let create_tode () =
  let x = T.create () in
  let xp = T.cos x in (* record DAG at construction *)
  { x; xp }

let ode = create_tode () in (* construct ode  *)
~~~
The Taylor expansion can then be computed and stored in `ode.x` using the following loop (here to the order 10):
~~~ocaml
let () = T.set ode.x 0 (OpFloat.make 1.) in (* set point of expansion *)
for i = 0 to 9 do
  ignore(T.eval ode.xp i); (* evaluate the i-th Taylor coefficient *)
  
  (* use dx/dt=ode(x)*)
  T.set ode.x (i+1) OpFloat.((T.deriv ode.xp i) / (integer Stdlib.(i+1)))
done;
~~~
Find the whole code [here](https://github.com/fadbadml-dev/FADBADml/blob/master/example/exampleTAD2.ml).

For more information about FAD, please refer to [FADBAD++ website].
