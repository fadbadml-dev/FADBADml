open Test

let pi = 3.14159265359

let test_pos =
  {
    uname = "pos";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(+x));
    uf = (fun x -> x);
    udfdx = (fun _ -> 1.);
  }

let test_neg =
  {
    uname = "neg";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(-x));
    uf = (fun x -> -.x);
    udfdx = (fun _ -> -1.);
  }

let test_inv =
  {
    uname = "inv";
    uarbitrary = non_zero_float;
    ufad = (fun x -> F.inv x);
    uf = (fun x -> 1. /. x);
    udfdx = (fun x -> -1. /. (x *. x));
  }

let test_sqr =
  {
    uname = "sqr";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.sqr x);
    uf = (fun x -> x *. x);
    udfdx = (fun x -> 2. *. x);
  }

let test_sqrt =
  {
    uname = "sqrt";
    uarbitrary = QCheck.pos_float;
    ufad = (fun x -> F.sqrt x);
    uf = sqrt;
    udfdx = (fun x -> 1. /. (2. *. sqrt x));
  }

let test_log =
  {
    uname = "log";
    uarbitrary = non_zero_pfloat;
    ufad = (fun x -> F.log x);
    uf = log;
    udfdx = (fun x -> 1. /. x);
  }

let test_sin =
  {
    uname = "sin";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.sin x);
    uf = sin;
    udfdx = cos;
  }

let test_cos =
  {
    uname = "cos";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.cos x);
    uf = cos;
    udfdx = (fun x -> -. sin x);
  }

let test_tan =
  {
    uname = "tan";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.tan x);
    uf = tan;
    udfdx = (fun x -> 1. +. (tan x) *. (tan x));
  }

let test_asin =
  {
    uname = "asin";
    uarbitrary = QCheck.float_range (-1.) 1.;
    ufad = (fun x -> F.asin x);
    uf = asin;
    udfdx = (fun x -> 1. /. (sqrt (1. -. x *. x)));
  }

let test_acos =
  {
    uname = "cos";
    uarbitrary = QCheck.float_range (-1.) 1.;
    ufad = (fun x -> F.acos x);
    uf = acos;
    udfdx = (fun x -> -1. /. (sqrt (1. -. x *. x)));
  }

let test_atan =
  {
    uname = "atan";
    uarbitrary = QCheck.float_range (-.pi /. 2.) (pi /. 2.);
    ufad = (fun x -> F.atan x);
    uf = atan;
    udfdx = (fun x -> 1. /. (1. +. x *. x));
  }

let test_addV =
  {
    uname = "addV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x +& 5.));
    uf = (fun x -> x +. 5.);
    udfdx = (fun _ -> 1.);
  }

let test_vAdd =
  {
    uname = "vAdd";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(5. &+ x));
    uf = (fun x -> 5. +. x);
    udfdx = (fun _ -> 1.);
  }

let test_cAddV =
  {
    uname = "cAddV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x +&= 5.));
    uf = (fun x -> x +. 5.);
    udfdx = (fun _ -> 1.);
  }

let test_subV =
  {
    uname = "subV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x -& 5.));
    uf = (fun x -> x -. 5.);
    udfdx = (fun _ -> 1.);
  }

let test_vSub =
  {
    uname = "vSub";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(5. &- x));
    uf = (fun x -> 5. -. x);
    udfdx = (fun _ -> -1.);
  }

let test_cSubV =
  {
    uname = "cSubV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x -&= 5.));
    uf = (fun x -> x -. 5.);
    udfdx = (fun _ -> 1.);
  }

let test_mulV =
  {
    uname = "mulV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x *& 5.));
    uf = (fun x -> x *. 5.);
    udfdx = (fun _ -> 5.);
  }

let test_vMul =
  {
    uname = "vMul";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(5. &* x));
    uf = (fun x -> 5. *. x);
    udfdx = (fun _ -> 5.);
  }

let test_cMulV =
  {
    uname = "cMulV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x *&= 5.));
    uf = (fun x -> x *. 5.);
    udfdx = (fun _ -> 5.);
  }

let test_divV =
  {
    uname = "divV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x /& 5.));
    uf = (fun x -> x /. 5.);
    udfdx = (fun _ -> 1. /. 5.);
  }

let test_vDiv =
  {
    uname = "vDiv";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(5. &/ x));
    uf = (fun x -> 5. /. x);
    udfdx = (fun x -> -5. /. (x *. x));
  }

let test_cDivV =
  {
    uname = "cDivV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x /&= 5.));
    uf = (fun x -> x /. 5.);
    udfdx = (fun _ -> 1. /. 5.);
  }

let test_powV =
  {
    uname = "powV";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(x **& 5.));
    uf = (fun x -> x ** 5.);
    udfdx = (fun x -> 5. *. x ** 4.);
  }

let test_vPow =
  {
    uname = "vPow";
    uarbitrary = QCheck.float;
    ufad = (fun x -> F.(5. &** x));
    uf = (fun x -> 5. ** x);
    udfdx = (fun x -> (log 5.) *. 5. ** x);
  }

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
  {
    bname = "add";
    barbitrary = QCheck.pair QCheck.float QCheck.float;
    bfad = (fun x y -> F.(x + y));
    bf = ( +. );
    bdfdx = (fun _ _ -> 1.);
    bdfdy = (fun _ _ -> 1.);
  }

let test_sub =
  {
    bname = "sub";
    barbitrary = QCheck.pair QCheck.float QCheck.float;
    bfad = (fun x y -> F.(x - y));
    bf = ( -. );
    bdfdx = (fun _ _ -> 1.);
    bdfdy = (fun _ _ -> -1.);
  }

let test_mul =
  {
    bname = "mul";
    barbitrary = QCheck.pair QCheck.float QCheck.float;
    bfad = (fun x y -> F.(x * y));
    bf = ( *. );
    bdfdx = (fun _ y -> y);
    bdfdy = (fun x _ -> x);
  }

let test_div =
  {
    bname = "div";
    barbitrary = QCheck.pair QCheck.float QCheck.float;
    bfad = (fun x y -> F.(x / y));
    bf = ( /. );
    bdfdx = (fun _ y -> 1. /. y);
    bdfdy = (fun x y -> -. x /. (y *. y));
  }

let test_pow =
  {
    bname = "pow";
    barbitrary = QCheck.pair (QCheck.float_range 0. 100.) (QCheck.float_range (-100.) 100.);
    bfad = (fun x y -> F.(x ** y));
    bf = ( ** );
    bdfdx = (fun x y -> y *. x ** (y -. 1.));
    bdfdy = (fun x y -> (log x) *. x ** y);
  }

let binary = [| test_add; test_sub; test_mul; test_div; test_pow |]

let test_all () =
  Random.self_init ();

  print_endline "---- UNARY FUNCTIONS";
  ignore (test_arr ~count:1000 unary);

  print_endline "\n---- BINARY FUNCTIONS";
  ignore (test2_arr ~count:1000 binary);

  ()

let _ = test_all ()
