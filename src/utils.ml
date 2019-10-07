let user_assert b s =
  if b then ()
  else begin Printf.eprintf "User assertion failed: %s\n" s; exit 1 end
let internal_assert b s =
  if b then ()
  else begin Printf.eprintf "Internal error: %s\n" s; exit 1 end
