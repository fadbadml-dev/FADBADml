module AAF = (AffineForm : Fadbad.OpS) (* test signature *)

open AffineForm

let print name i =
  Printf.printf "%s = %s\n" name (to_string i)

let () =
  let e = Array.init 3 (fun _ -> create_noise ()) in
  let aaf1 = (make_float 3.) + (scale e.(0) 2.) - e.(2) in
  let aaf2 = (make_float 1.) - e.(0) + (scale e.(1) 3.) in
  let () = print "aaf1" aaf1 in
  let () = print "aaf2" aaf2 in
  let () = print "aaf1 + aaf2" (aaf1 + aaf2) in
  let () = print "aaf1 - aaf2" (aaf1 - aaf2) in
  let () = print "aaf1 * aaf2" (aaf1 * aaf2) in
  let () = print "aaf1 / aaf2" (aaf1 / aaf2) in
  let () = print "aaf2 / aaf1" (aaf2 / aaf1) in
  ()
