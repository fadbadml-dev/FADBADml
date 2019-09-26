module Op = Fadbad.Op.OpFloat
module F = Fadbad.F(Op)

let func x y =
  let open F in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let x = F.make 1. in
  let () = F.diff x 0 2 in
  let y = F.make 2. in
  let () = F.diff y 1 2 in
  let f  = F.func x y in
  let f_val = F.get f in
  let dfdx = F.d f 0 in
  let dfdy = F.d f 1 in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  ()
