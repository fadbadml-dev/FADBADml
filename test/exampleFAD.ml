open Fadbad

module MyF = Fadiff.FTypeName(Op.OpFloat)
open MyF

let func x y =
  let z = sqrt x in
  (y * z) + (sin x)

let () =
  let x = MyF.make 1. in
  let () = diff x 0 2 in
  let y = make 2. in
  let () = diff y 1 2 in
  let f  = func x y in
  let f_val = f.m_val in
  let dfdx = d f 0 in
  let dfdy = d f 1 in
  let () = print_newline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_newline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_newline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  ()
