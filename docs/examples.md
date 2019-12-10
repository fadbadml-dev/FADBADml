<!--
FADBADml: examples
Ismail Bennani; François Bidet
-->

[home]: index.html
[Fadbad.OpS]: doc/Fadbad.OpS.html
[Fadbad.OpFloat]: doc/Fadbad.OpFloat.html
[Fadbad.F]: doc/Fadbad.F.html
[Fadbad.B]: doc/Fadbad.B.html
[Fadbad.T]: doc/Fadbad.T.html

## Overloading and templates (C++) vs. Modules and Functors (OCaml)

FADBAD++ relies on operator overloading in C++ to compute the derivatives of an expression along with its value. In OCaml there is no such thing, we define instead modules that re-implement the classic arithmetic functions (+, *, sin, ...).
To replace the templates used in FADBAD++ to define `F<.>`, `B<.>` and `T<.>`, we use functors `Fadbad.F(.)`, `Fadbad.B(.)` and `Fadbad.T(.)` that construct the arithmetic functions using the same functions over the underlying type.
For example, the FADBAD++ type `F<float>` would be `Fadbad.F(Fadbad.OpFloat).t` in FADBADml, where `Fabdad.OpFloat` is a module defined in FADBADml that gives the implementations of the arithmetic functions for the type `float` (ie. the ones in `Pervasives`).

## Quick-start

File `exampleFAD.ml` (this can be found in subfolder `examples` in our repo):
~~~ocaml
module Op = Fadbad.OpFloat (* elementary floating point arithmetic *)
module F = Fadbad.F(Op) (* equivalent of F<float> class *)

let func x y =
  let open F in (* to avoid adding `F.` in front of every function *)
  (* the functions `sqrt`, `*`, `+` and `sin` below are the ones defined in F *)
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  (* `F.make` converts a float into a value of type F.t *)
  let x = F.make 1. in
  (* `F.diff x 0 2` means that we want to differentiate wrt. `x`,
     that the index of `x` will be 0 and that we will
     differentiate wrt. 2 variables at most *)
  let () = F.diff x 0 2 in

  (* same for y *)
  let y = F.make 2. in
  let () = F.diff y 1 2 in

  (* by applying `f`, we apply the operators in `F` hence computing
     the value of the expression and its derivatives wrt. `x` and `y` *)
  let f  = func x y in

  (* get the value of the result *)
  let f_val = F.get f in

  (* get the derivatives wrt. variable 0 (ie `x`) *)
  let dfdx = F.d f 0 in
  (* get the derivatives wrt. variable 1 (ie `y`) *)
  let dfdy = F.d f 1 in

  (* print stuff *)
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  ()
~~~

Then compile this code with
``ocamlc -o exampleFAD -I `ocamlfind query fadbadml` fadbad.cma exampleFAD.ml``

## Forward automatic differentiation (FAD)

See [Fadbad.F]

In its forward mode, FADBADml does not compute the graph of the expressions explicitly. The type `F(Op).t` is the following record:

~~~ocaml
(* Op is the argument given to the functor F *)
type t = {
    m_val : Op.t;
    m_diff : Op.t array;
}
~~~

It keeps track of the value of the variable and its derivatives.

Let's assume you have an existing function to which you want to apply FAD:

~~~ocaml
(* x and y are floats here and sqrt, *.
   and sin are the ones from Pervasives *)
let func x y =
  let z = sqrt x in
  (y *. z) +. (sin z)
~~~

You need to replace the arithmetic functions by the ones in F, one way to do it is to re-write your function as:

~~~ocaml
module Op = Fadbad.OpFloat (* contains floating point arithmetic functions
                              (the ones in Pervasives) *)
module F = Fadbad.F(Op)    (* equivalent of F<float> in FADBAD++ *)

(* x and y are floats here and sqrt, *.
   and sin are the ones from Pervasives *)
let func x y =
  let open F in
  let z = sqrt x in
  (y * z) + (sin z)
~~~

Note that in F, by default, the operators do not have a `.` suffix like the float operators in OCaml, we did not want to shadow these because in our use cases, we found it useful to be able to do something like `scale (x*y) (2. *. pi)` where `pi` is of type `float`. However, it would be easy to write a new functor `MyF` that aliases the operators in `F` so that they shadow the float operators.

Once we have the function we want to differentiate, we need to give it inputs:

~~~ocaml
...
let x = F.make 1. in
let y = F.make 3. in
...
~~~

`F.make` turns a float value (type `Op.t`) into an `F.t` value. At this point, the array of derivatives is empty.
We need to tell the library the variables that we want to use for the differentiation:

~~~ocaml
...
let () = F.diff x 0 2 in
let () = F.diff y 1 2 in
...
~~~

<!--
  Pour un rendu plus joli, j'utilise &nbsp; qui est le code HTML pour l'espace insécable, ça rend le code plutôt illisible ici mais ça évite que les morceaux de code inlinés soient coupés sur plusieurs lignes
-->

`F.diff` builds the array of derivatives for the variables wrt. which we differentiate: `F.diff&nbsp;x&nbsp;0&nbsp;2` builds the array `[|&nbsp;1;&nbsp;0&nbsp;|]` for x, which means that `dx/dx&nbsp;=&nbsp;1` and `dx/dy&nbsp;=&nbsp;0`.
Then we can call `func` and retrieve the output value and derivatives:

~~~ocaml
...
(* by applying `f`, we apply the operators in `F` hence we compute
   the value of the expression and its derivatives wrt. `x` and `y` *)
let f  = func x y in

(* get the value of the result *)
let f_val = F.get f in

(* get the derivatives wrt. variable 0 (ie `x`) *)
let dfdx = F.d f 0 in
(* get the derivatives wrt. variable 1 (ie `y`) *)
let dfdy = F.d f 1 in
...
~~~

Find the whole code [here](#quick-start).
For more information about FAD, please refer to [FADBAD++ website](http://www.fadbad.com/fadbad.html#CrashCourse).

## Backward automatic differentiation

See [Fadbad.B]

## Automatic Taylor expansion

See [Fadbad.T]
