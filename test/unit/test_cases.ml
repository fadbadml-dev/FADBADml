(* preset arbitraries *)

let non_zero_float : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec non_zero_float_gen rand =
       let f = QCheck.Gen.float rand in
       if f = 0. then non_zero_float_gen rand
       else f
     in non_zero_float_gen)

let non_zero_pfloat : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec non_zero_pfloat_gen rand =
       let f = QCheck.Gen.pfloat rand in
       if f = 0. then non_zero_pfloat_gen rand
       else f
     in non_zero_pfloat_gen)

let int_float : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec int_float_gen rand =
       let f = QCheck.Gen.int rand in float f
     in int_float_gen)

let int_float_range low high : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec int_float_range_gen rand =
       let f = QCheck.Gen.int_range low high rand in float f
     in int_float_range_gen)

(* TEST CASES *)

module Make(Op : Fadbad.OpS) =
struct

  type unary_test = {
    uname : string;
    udesc : string;
    uarbitrary : float QCheck.arbitrary;
    ufad : Op.t -> Op.t;
    uf : float -> float;
    udfdx : float -> float;
  }

  type binary_test = {
    bname : string;
    bdesc : string;
    barbitrary : (float * float) QCheck.arbitrary;
    bfad : Op.t -> Op.t -> Op.t;
    bf : float -> float -> float;
    bdfdx : float -> float -> float;
    bdfdy : float -> float -> float;
  }

  let pi = 3.14159265359

  let test_pos =
    {
      uname = "pos";
      udesc = "f(x) = + x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(+x));
      uf = (fun x -> x);
      udfdx = (fun _ -> 1.);
    }

  let test_neg =
    {
      uname = "neg";
      udesc = "f(x) = - x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(-x));
      uf = (fun x -> -.x);
      udfdx = (fun _ -> -1.);
    }

  let test_inv =
    {
      uname = "inv";
      udesc = "f(x) = 1 / x";
      uarbitrary = non_zero_float;
      ufad = (fun x -> Op.inv x);
      uf = (fun x -> 1. /. x);
      udfdx = (fun x -> -1. /. (x *. x));
    }

  let test_sqr =
    {
      uname = "sqr";
      udesc = "f(x) = x * x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sqr x);
      uf = (fun x -> x *. x);
      udfdx = (fun x -> 2. *. x);
    }

  let test_sqrt =
    {
      uname = "sqrt";
      udesc = "f(x) = sqrt x";
      uarbitrary = QCheck.pos_float;
      ufad = (fun x -> Op.sqrt x);
      uf = sqrt;
      udfdx = (fun x -> 1. /. (2. *. sqrt x));
    }

  let test_log =
    {
      uname = "log";
      udesc = "f(x) = log x";
      uarbitrary = non_zero_pfloat;
      ufad = (fun x -> Op.log x);
      uf = log;
      udfdx = (fun x -> 1. /. x);
    }

  let test_sin =
    {
      uname = "sin";
      udesc = "f(x) = sin x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sin x);
      uf = sin;
      udfdx = cos;
    }

  let test_cos =
    {
      uname = "cos";
      udesc = "f(x) = cos x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.cos x);
      uf = cos;
      udfdx = (fun x -> -. sin x);
    }

  let test_tan =
    {
      uname = "tan";
      udesc = "f(x) = tan x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.tan x);
      uf = tan;
      udfdx = (fun x -> 1. +. (tan x) *. (tan x));
    }

  let test_asin =
    {
      uname = "asin";
      udesc = "f(x) = asin x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.asin x);
      uf = asin;
      udfdx = (fun x -> 1. /. (sqrt (1. -. x *. x)));
    }

  let test_acos =
    {
      uname = "acos";
      udesc = "f(x) = acos x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.acos x);
      uf = acos;
      udfdx = (fun x -> -1. /. (sqrt (1. -. x *. x)));
    }

  let test_atan =
    {
      uname = "atan";
      udesc = "f(x) = atan x";
      uarbitrary = QCheck.float_range (-.pi /. 2.) (pi /. 2.);
      ufad = (fun x -> Op.atan x);
      uf = atan;
      udfdx = (fun x -> 1. /. (1. +. x *. x));
    }

  let unary = [|
    test_pos; test_neg;
    test_inv; test_sqr; test_sqrt; test_log;
    test_sin; test_cos; test_tan;
    test_asin; test_acos; test_atan;
  |]

  let test_add =
    {
      bname = "add";
      bdesc = "f(x, y) = x + y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x + y));
      bf = ( +. );
      bdfdx = (fun _ _ -> 1.);
      bdfdy = (fun _ _ -> 1.);
    }

  let test_cAdd =
    {
      bname = "cAdd";
      bdesc = "f(x, y) = x += y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x += y));
      bf = ( +. );
      bdfdx = (fun _ _ -> 1.);
      bdfdy = (fun _ _ -> 1.);
    }

  let test_sub =
    {
      bname = "sub";
      bdesc = "f(x, y) = x - y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x - y));
      bf = ( -. );
      bdfdx = (fun _ _ -> 1.);
      bdfdy = (fun _ _ -> -1.);
    }

  let test_cSub =
    {
      bname = "cSub";
      bdesc = "f(x, y) = x -= y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x -= y));
      bf = ( -. );
      bdfdx = (fun _ _ -> 1.);
      bdfdy = (fun _ _ -> -1.);
    }

  let test_mul =
    {
      bname = "mul";
      bdesc = "f(x, y) = x * y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x * y));
      bf = ( *. );
      bdfdx = (fun _ y -> y);
      bdfdy = (fun x _ -> x);
    }

  let test_cMul =
    {
      bname = "cMul";
      bdesc = "f(x, y) = (x *= y)";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x *= y));
      bf = ( *. );
      (* bdfdx = (fun _ y -> y); *)
      bdfdx = (fun _ y -> 1.);
      bdfdy = (fun x _ -> x);
    }

  let test_div =
    {
      bname = "div";
      bdesc = "f(x, y) = x / y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x / y));
      bf = ( /. );
      bdfdx = (fun _ y -> 1. /. y);
      bdfdy = (fun x y -> -. x /. (y *. y));
    }

  let test_cDiv =
    {
      bname = "cDiv";
      bdesc = "f(x, y) = (x /= y)";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x /= y));
      bf = ( /. );
      (* bdfdx = (fun _ y -> 1. /. y); *)
      bdfdx = (fun _ y -> 1.);
      bdfdy = (fun x y -> -. x /. (y *. y));
    }

  let test_pow =
    {
      bname = "pow";
      bdesc = "f(x, y) = x ^ y";
      barbitrary = QCheck.pair (QCheck.float_range 0. 100.) (QCheck.float_range (-100.) 100.);
      bfad = (fun x y -> Op.(x ** y));
      bf = ( ** );
      bdfdx = (fun x y -> y *. x ** (y -. 1.));
      bdfdy = (fun x y -> (log x) *. x ** y);
    }

  let binary = [| test_add; test_sub; test_mul; test_div;
                  test_cAdd; test_cSub; test_cMul; test_cDiv;
                  test_pow |]
end
