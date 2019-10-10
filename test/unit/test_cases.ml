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

let rec fact n = if n = 0 then 1 else n * (fact (n-1))
let rec pochhammer f i = if i = 0 then 1. else f *. (pochhammer (f+.1.) (i-1))

module Make(Op : Fadbad.OpS) =
struct

  type unary_test = {
    uname : string;
    udesc : string;
    uarbitrary : float QCheck.arbitrary;
    ufad : Op.t -> Op.t;
    (* udfdx i x = d^if/dx^i x for i >= 1 *)
    udfdx : int -> float -> float;
  }

  type binary_test = {
    bname : string;
    bdesc : string;
    barbitrary : (float * float) QCheck.arbitrary;
    bfad : Op.t -> Op.t -> Op.t;
    (* udfdx i x = d^if/dx^i x for i >= 1 *)
    bdfdx : int -> float -> float -> float;
    (* udfdy i x = d^if/dy^i x for i >= 1 *)
    bdfdy : int -> float -> float -> float;
  }

  let pi = 3.14159265359

  let test_pos =
    {
      uname = "pos";
      udesc = "f(x) = + x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(+x));
      udfdx = (fun i x -> match i with 0 -> x | 1 -> 1. | _ -> 0.);
    }

  let test_neg =
    {
      uname = "neg";
      udesc = "f(x) = - x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(-x));
      udfdx = (fun i x -> match i with 0 -> -. x | 1 -> -1. | _ -> 0.);
    }

  let test_inv =
    {
      uname = "inv";
      udesc = "f(x) = 1 / x";
      uarbitrary = non_zero_float;
      ufad = (fun x -> Op.inv x);
      udfdx = (fun i x ->
        (-1. ** (float i)) *. (float (fact i)) /. (x ** (float (i+1))));
    }

  let test_sqr =
    {
      uname = "sqr";
      udesc = "f(x) = x * x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sqr x);
      udfdx = (fun i x ->
                match i with
                | 0 -> x *. x
                | 1 -> 2. *. x
                | 2 -> 2.
                | _ -> 0.);
    }

  let test_sqrt =
    {
      uname = "sqrt";
      udesc = "f(x) = sqrt x";
      uarbitrary = QCheck.pos_float;
      ufad = (fun x -> Op.sqrt x);
      udfdx = (fun i x ->
        (-1. ** (float i)) *. (pochhammer (-0.5) i) *.
        (x ** (0.5 -. (float i))))
    }

  let test_log =
    {
      uname = "log";
      udesc = "f(x) = log x";
      uarbitrary = non_zero_pfloat;
      ufad = (fun x -> Op.log x);
      udfdx = (fun i x ->
                match i with
                | 0 -> log x
                | _ -> test_inv.udfdx (i-1) x);
    }

  let test_sin =
    {
      uname = "sin";
      udesc = "f(x) = sin x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sin x);
      udfdx =
        let aux i x =
          match i with
          | 0 -> sin x
          | 1 -> cos x
          | 2 -> -. sin x
          | 3 -> -. cos x
          | _ -> assert false
        in (fun i x -> aux (i mod 4) x);
    }

  let test_cos =
    {
      uname = "cos";
      udesc = "f(x) = cos x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.cos x);
      udfdx = (fun i x ->
                match i with
                | 0 -> cos x
                | _ -> -. test_sin.udfdx (i-1) x);
    }

  let test_tan =
    {
      uname = "tan";
      udesc = "f(x) = tan x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.tan x);
      udfdx =
        let rec tan_der i x =
          match i with
          | 0 -> tan x
          | 1 -> 1. +. (tan x) *. (tan x)
          | _ -> (float i) *. (1. +. (tan x) *. (tan x)) *. (tan_der (i-1) x)
        in tan_der;
    }

  let test_asin =
    {
      uname = "asin";
      udesc = "f(x) = asin x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.asin x);
      udfdx = (fun i x ->
        match i with
        | 0 -> asin x
        | 1 -> 1./.(sqrt (1.-.(x**2.)))
        | 2 -> x/.((1.-.(x**2.))**1.5)
        | 3 -> (2.*.(x**2.)+.1.)/.((1.-.(x**2.))**2.5)
        | 4 -> (3.*.x*.(2.*.(x**2.)+.3.))/.((1.-.(x**2.))**2.5)
        | _ -> Printf.eprintf "derivative %d of asin/acos not defined" i; exit 1
      );
    }

  let test_acos =
    {
      uname = "acos";
      udesc = "f(x) = acos x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.acos x);
      udfdx = (fun i x ->
        match i with
        | 0 -> acos x
        | _ -> -. test_asin.udfdx i x
      );
    }

  let test_atan =
    {
      uname = "atan";
      udesc = "f(x) = atan x";
      uarbitrary = QCheck.float_range (-.pi /. 2.) (pi /. 2.);
      ufad = (fun x -> Op.atan x);
      udfdx =
        (* atan_der i x computes the i-th and (i+1)-th derivatives of atan *)
        let rec atan_der i x =
          match i with
          | 0 -> (atan x, 0.)
          | 1 -> (1. /. (1. +. x *. x), - 2. *. x /. ((1. +. x *. x) ** 2.))
          | _ ->
            let n = float i in
            let der_im1, der_i = atan_der (i-1) x in
            (der_i,
            (2.*.(n+.1.)*.x*.der_i +. (n+.1.)*.n*.der_im1) /. (1. +. x *. x))
        in (fun i x -> fst (atan_der i x));
    }

  let unary = [|
    test_pos; test_neg;
    test_inv; test_sqr; test_sqrt; test_log;
    test_sin; test_cos; test_tan;
    test_asin; test_acos; test_atan;
  |]

  let test_add =
    let der i x y = match i with | 0 -> x +. y | 1 -> 1. | _ -> 0. in
    {
      bname = "add";
      bdesc = "f(x, y) = x + y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x + y));
      bdfdx = der;
      bdfdy = der;
    }

  let test_sub =
    let der a i x y = match i with | 0 -> x -. y | 1 -> a | _ -> 0. in
    {
      bname = "sub";
      bdesc = "f(x, y) = x - y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x - y));
      bdfdx = der 1.;
      bdfdy = der (-1.);
    }

  let test_mul =
    {
      bname = "mul";
      bdesc = "f(x, y) = x * y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x * y));
      bdfdx = (fun i x y -> (test_pos.udfdx i x) *. y);
      bdfdy = (fun i x y -> x *. (test_pos.udfdx i y));
    }

  let test_div =
    {
      bname = "div";
      bdesc = "f(x, y) = x / y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x / y));
      bdfdx = (fun i x y -> (test_pos.udfdx i x) /. y);
      bdfdy = (fun i x y -> x *. (test_inv.udfdx i y));
    }

  let test_pow =
    {
      bname = "pow";
      bdesc = "f(x, y) = x ^ y";
      barbitrary = QCheck.pair (QCheck.float_range 0. 100.)
                               (QCheck.float_range (-100.) 100.);
      bfad = (fun x y -> Op.(x ** y));
      bdfdx = (fun i x y ->
        (pochhammer (1. -. (float i) +. y) i) *. x ** (y -. (float i)));
      bdfdy = (fun i x y -> ((log x) ** (float i)) *. x ** y);
    }

  let binary = [| test_add; test_sub; test_mul; test_div; test_pow |]
end
