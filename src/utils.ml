let user_assert b s =
  if b then () else Printf.eprintf "User assertion failed: %s\n" s
let internal_assert b s =
  if b then () else Printf.eprintf "Internal error: %s\n" s
