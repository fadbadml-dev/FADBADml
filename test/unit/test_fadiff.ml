open Test

let pi = 3.14159265359

let test_pos =
  "pos",
  QCheck.float,
  compare (fun x -> F.(+x))
    (fun x -> x) (fun _ -> 1.)

let test_neg =
  "neg",
  QCheck.float,
  compare (fun x -> F.(-x))
    (fun x -> -.x) (fun _ -> -1.)

let test_inv =
  "inv",
  non_zero_float,
  compare (fun x -> F.inv x)
    (fun x -> 1. /. x) (fun x -> -1. /. (x *. x))

let test_sqr =
  "sqr",
  QCheck.float,
  compare (fun x -> F.sqr x)
    (fun x -> x *. x) (fun x -> 2. *. x)

let test_sqrt =
  "sqrt",
  QCheck.pos_float,
  compare (fun x -> F.sqrt x)
    sqrt (fun x -> 1. /. (2. *. sqrt x))

let test_log =
  "log",
  non_zero_pfloat,
  compare (fun x -> F.log x)
    log (fun x -> 1. /. x)

let test_sin =
  "sin",
  QCheck.float,
  compare (fun x -> F.sin x)
    sin cos

let test_cos =
  "cos",
  QCheck.float,
  compare (fun x -> F.cos x)
    cos (fun x -> -. sin x)

let test_tan =
  "tan",
  QCheck.float,
  compare (fun x -> F.tan x)
    tan (fun x -> 1. +. (tan x) *. (tan x))

let test_asin =
  "asin",
  QCheck.float_range (-1.) 1.,
  compare (fun x -> F.asin x)
    asin (fun x -> 1. /. (sqrt (1. -. x *. x)))

let test_acos =
  "cos",
  QCheck.float_range (-1.) 1.,
  compare (fun x -> F.acos x)
    acos (fun x -> -1. /. (sqrt (1. -. x *. x)))

let test_atan =
  "atan",
  QCheck.float_range (-.pi /. 2.) (pi /. 2.),
  compare (fun x -> F.atan x)
    atan (fun x -> 1. /. (1. +. x *. x))

let test_addV =
  "addV",
  QCheck.float,
  compare (fun x -> F.(x +& 5.))
    (fun x -> x +. 5.) (fun _ -> 1.)

let test_vAdd =
  "vAdd",
  QCheck.float,
  compare (fun x -> F.(5. &+ x))
    (fun x -> 5. +. x) (fun _ -> 1.)

let test_cAddV =
  "cAddV",
  QCheck.float,
  compare (fun x -> F.(x +&= 5.))
    (fun x -> x +. 5.) (fun _ -> 1.)

let test_subV =
  "subV",
  QCheck.float,
  compare (fun x -> F.(x -& 5.))
    (fun x -> x -. 5.) (fun _ -> 1.)

let test_vSub =
  "vSub",
  QCheck.float,
  compare (fun x -> F.(5. &- x))
    (fun x -> 5. -. x) (fun _ -> -1.)

let test_cSubV =
  "cSubV",
  QCheck.float,
  compare (fun x -> F.(x -&= 5.))
    (fun x -> x -. 5.) (fun _ -> 1.)

let test_mulV =
  "mulV",
  QCheck.float,
  compare (fun x -> F.(x *& 5.))
    (fun x -> x *. 5.) (fun _ -> 5.)

let test_vMul =
  "vMul",
  QCheck.float,
  compare (fun x -> F.(5. &* x))
    (fun x -> 5. *. x) (fun _ -> 5.)

let test_cMulV =
  "cMulV",
  QCheck.float,
  compare (fun x -> F.(x *&= 5.))
    (fun x -> x *. 5.) (fun _ -> 5.)

let test_divV =
  "divV",
  QCheck.float,
  compare (fun x -> F.(x /& 5.))
    (fun x -> x /. 5.) (fun _ -> 1. /. 5.)

let test_vDiv =
  "vDiv",
  QCheck.float,
  compare (fun x -> F.(5. &/ x))
    (fun x -> 5. /. x) (fun x -> -5. /. (x *. x))

let test_cDivV =
  "cDivV",
  QCheck.float,
  compare (fun x -> F.(x /&= 5.))
    (fun x -> x /. 5.) (fun _ -> 1. /. 5.)

let test_powV =
  "powV",
  QCheck.float,
  compare (fun x -> F.(x **& 5.))
    (fun x -> x ** 5.) (fun x -> 5. *. x ** 4.)

let test_vPow =
  "vPow",
  QCheck.float,
  compare (fun x -> F.(5. &** x))
    (fun x -> 5. ** x) (fun x -> (log 5.) *. 5. ** x)

let unary = [|
  test_pos; test_neg;
  test_inv; test_sqr; test_sqrt; test_log;
  test_sin; test_cos; test_tan;
  test_asin; test_acos; test_atan;
  test_addV; test_vAdd; test_cAddV;
  test_subV; test_vSub; test_cSubV;
  test_mulV; test_vMul; test_cMulV;
  test_divV; test_vDiv; test_cDivV;
  test_powV; test_vPow;
|]

let test_add =
  "add",
  QCheck.pair QCheck.float QCheck.float,
  compare2 (fun x y -> F.(x + y))
    ( +. ) (fun _ _ -> 1.) (fun _ _ -> 1.)

let binary = [|
  test_add
|]

let _ =
  Random.self_init ();

  print_endline "---- UNARY FUNCTIONS";
  ignore (test_unary_arr ~count:1000 unary);

  print_endline "\n---- BINARY FUNCTIONS";
  ignore (test_binary_arr ~count:1000 binary);

  ()
