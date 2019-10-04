module Op = Op.OpFloat
module B = Fadbad.B(Op)

let show_der pref der =
  Printf.printf "%s: [%s]\n" pref (String.concat ", " (Array.to_list (Array.map string_of_float (Array.map Op.get der))))

let _ =
  let x = B.make 5. in
  let y = B.make 12. in
  let z = B.make 2. in
  let f = B.(x + y + z) in
  let g = B.(y * z) in
  B.diff f 0 2;
  B.diff g 1 2;
  Printf.printf "df/dx = %f\n" (B.d x 0);
  Printf.printf "df/dy = %f\n" (B.d y 0);
  Printf.printf "df/dz = %f\n" (B.d z 0);
  Printf.printf "dg/dx = %f\n" (B.d x 1);
  Printf.printf "dg/dy = %f\n" (B.d y 1);
  Printf.printf "dg/dz = %f\n" (B.d z 1);
  ()
