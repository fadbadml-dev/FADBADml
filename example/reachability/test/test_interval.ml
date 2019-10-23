module Int = (Interval : Fadbad.OpS) (* test signature *)

open Interval

let print name i =
  Printf.printf "%s = %s\n" name (to_string i)

let () =
  let i1 = make { min = 2.; max = 5. } in
  let i2 = make { min = -1.; max = 2. } in
  let () = print "i1" i1 in
  let () = print "i2" i2 in
  let () = print "i1 + i2" (i1 + i2) in
  let () = print "i1 - i2" (i1 - i2) in
  let () = print "i1 * i2" (i1 * i2) in
  let () = print "i1 / i2" (i1 / i2) in
  let () = print "i2 / i1" (i2 / i1) in
  ()
