open Test

let test_inv =
  "test_inv",
  non_zero_float,
  compare (fun x -> F.inv x)
    (fun x -> 1. /. x) (fun x -> -1. /. (x *. x))

let test_sqr =
  "test_sqr",
  QCheck.float,
  compare (fun x -> F.sqr x)
    (fun x -> x *. x) (fun x -> 2. *. x)

let test_sqrt =
  "test_sqrt",
  QCheck.pos_float,
  compare (fun x -> F.sqrt x)
    (fun x -> sqrt x) (fun x -> 1. /. (2. *. sqrt x))

let test_sin =
  "test_sin",
  QCheck.float,
  compare (fun x -> F.sin x)
    sin cos

let test_cos =
  "test_cos",
  QCheck.float,
  compare (fun x -> F.cos x)
    cos (fun x -> -. sin x)

let tests = [|
  test_inv; test_sqr; test_sqrt; test_sin; test_cos
|]

let _ =
  test_all ~count:1000 tests
