<!--
FADBADml: examples
Ismail Bennani; François Bidet
-->

[home]: ../../index.html
[Fadbad.OpS]: ../../doc/Fadbad.OpS.html
[Fadbad.OpFloat]: ../../doc/Fadbad.OpFloat.html
[Fadbad.F]: ../../doc/Fadbad.F.html
[Fadbad.B]: ../../doc/Fadbad.B.html
[Fadbad.T]: ../../doc/Fadbad.T.html

## Module types instead of primitive types

To use FADBADml, we need a module defining the type we want to use and implemented the interface [Fadbad.OpS].
If we want to use the `type float`, we can use the pre-defined module [Fadbad.OpFloat].

Then we have to wrap our module in another using the desired functor:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ocaml
module MyF = Fadbad.F(MyModule) (* Fadiff: forward *)
module MyB = Fadbad.B(MyModule) (* Badiff: backward *)
module MyT = Fadbad.T(MyModule) (* Tadiff: Taylor *)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Forward automatic differentiation

See [Fadbad.F]

Forward automatic differentiation consists of propagating derivatives from input to output.

There is no tree stored in memory. Every variable is a record containing the value and the derivatives :
~~~ocaml
type 'a t = {
    m_val : 'a;
    m_diff : 'a array;
}
~~~

If we want to derive an expression `z` with respect to a variable `x`, then we have to set the derivative of `x` to `1` before computing `z` :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ocaml
F.diff x i n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with `x` the input variable, `i` the index of the corresponding derivative, and `n` the total number of desired derivatives (with `0 <= i < n`).

In the following example, we compute the derivatives of `5x + y²` with respect to `x` and with respect to `y`. We so have `n = 2` and we decide the index of the derivative with respect to `x` will be `0` and the index of the one with respect to `y` will be `1`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ocaml
module Op = Fadbad.OpFloat (* pre-defined module using the type float*)
module F = Fadbad.F(Op)    (* forward module using floats number*)

let () =
  let x = F.make 1. in               (* input variable [x]: (1., []) *)
  let () = F.diff x 0 2 in           (* [x]: (1., [1., 0.]) *)
  let y = F.make 3. in               (* input variable [y]: (3., []) *)
  let () = F.diff y 1 2 in           (* [y]: (3., [0., 1.]) *)
  let z  = (scale 5 x) + (sqr y) in  (* variable [z]: (14., [5., 6.]) *)
  print_string "z = "; print_float (F.get z);     (* print: "z = 14." *)
  print_newline ();
  print_string "dz/dx = "; print_float (F.d z 0); (* print: "dz/dx = 5." *)
  print_newline ();
  print_string "dz/dy = "; print_float (F.d z 1); (* print: "dz/dy = 6." *)
  ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Backward automatic differentiation

See [Fadbad.B]

## Automatic Taylor expansion

See [Fadbad.T]
