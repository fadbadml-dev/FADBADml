open Fadbad

module Op = Op.OpFloat
module MyF = F(Op)

let func x y =
  let open MyF in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let open MyF in
  let x = make (Op.make 1.) in
  let () = diff x 0 2 in
  let y = make (Op.make 2.) in
  let () = diff y 1 2 in
  let f  = func x y in
  let f_val = Op.get f.m_val in
  let dfdx = Op.get (d f 0) in
  let dfdy = Op.get (d f 1) in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  ()
