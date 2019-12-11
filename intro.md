[git]: https://github.com/fadbadml-dev/FADBADml

## Summary

<div id="summary" class="centered" markdown="1">
[Quick-start](#quick-start)
[Forward automatic differentiation (FAD)](#FAD)
[Backward automatic differentiation (BAD)](#BAD)
[Automatic Taylor expansion (TAD)](#TAD)
</div>

## Overloading and templates (C++) vs. Modules and Functors (OCaml)

FADBAD++ relies on operator overloading in C++ to compute the derivatives of an expression along with its value. In OCaml there is no such thing, we define instead modules that re-implement the classic arithmetic functions (+, *, sin, ...).
To replace the templates used in FADBAD++ to define `F<.>`, `B<.>` and `T<.>`, we use functors `Fadbad.F(.)`, `Fadbad.B(.)` and `Fadbad.T(.)` that construct the arithmetic functions using the same functions over the underlying type.
For example, the FADBAD++ type `F<float>` would be `Fadbad.F(Fadbad.OpFloat).t` in FADBADml, where `Fabdad.OpFloat` is a module defined in FADBADml that gives the implementations of the arithmetic functions for the type `float` (ie. the ones in `Pervasives`).

## Quick-start

File `exampleFAD.ml` (this can be found in subfolder `examples` in our [git]):

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

Then compile with
``ocamlc -o exampleFAD -I `ocamlfind query fadbadml` fadbad.cma exampleFAD.ml``
