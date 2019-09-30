module Op = Fadbad.Op.OpFloat
module F = Fadbad.F(Fadbad.F(Op))

let func x y =
  let open F in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let x = F.make 1. in
  let () = F.diff_n x 0 2 2 in
  let y = F.make 2. in
  let () = F.diff_n y 1 2 2 in
  (* let () = F.diff y 1 2 in *)
  let f  = func x y in
  let f_val = F.get f in
  let dfdx = F.d f 0 in
  let dfdxdx = F.d_n f [0; 0] in
  let dfdydx = F.d_n f [1; 0] in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dxdx(x,y) = " ^ (string_of_float dfdxdx)) in
  let () = print_endline ("df/dydx(x,y) = " ^ (string_of_float dfdydx)) in
  ()
