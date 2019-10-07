open Fadbad

(* utils *)

let eq_float a b epsilon =
  let absA = abs_float a in
  let absB = abs_float b in
  let diff = abs_float (a -. b) in

  if a = b then
    (* shortcut, handles infinities *)
    true
  else if Float.is_nan a then Float.is_nan b
  else if Float.is_nan b then Float.is_nan a
  else if a = 0. || b = 0. || (absA +. absB < epsilon_float) then
    (* a or b is zero or both are extremely close to it
       relative error is less meaningful here *)
    diff < (epsilon *. epsilon_float)
  else
    diff /. (min (absA +. absB) max_float) < epsilon

let eq_float a b = eq_float a b 1e-10

let check_results results =
  Array.for_all (fun (cell, res) ->
      match res.QCheck.TestResult.state with
      | Success -> true
      | Error _ | Failed _ | Failed_other _ -> false
    ) results


(* main test functions *)

module type Compute =
sig
  type unary
  type binary
  type value

  val unary_name : unary -> string
  val unary_desc : unary -> string
  val unary_arbitrary : unary -> float QCheck.arbitrary
  val unary_fad : unary -> (value -> value)
  val unary_f : unary -> (float -> float)
  val unary_dfdx : unary -> (float -> float)

  val binary_name : binary -> string
  val binary_desc : binary -> string
  val binary_arbitrary : binary -> (float * float) QCheck.arbitrary
  val binary_fad : binary -> (value -> value -> value)
  val binary_f : binary -> (float -> float -> float)
  val binary_dfdx : binary -> (float -> float -> float)
  val binary_dfdy : binary -> (float -> float -> float)

  val compute_unary : unary -> float -> (float * float) * (float * float)
  val compute_binary : binary -> float * float -> (float * float) * (float * float) * (float * float)

  val unary_tests : unary array
  val binary_tests : binary array
end

module ComputeFAD =
struct
  module Op = F(OpFloat)
  module TestCases = Test_cases.Make(Op)

  type unary = TestCases.unary_test
  type binary = TestCases.binary_test
  type value = Op.t

  let unary_name u = u.TestCases.uname
  let unary_desc u = u.TestCases.udesc
  let unary_arbitrary u = u.TestCases.uarbitrary
  let unary_fad u = u.TestCases.ufad
  let unary_f u = u.TestCases.uf
  let unary_dfdx u = u.TestCases.udfdx

  let binary_name b = b.TestCases.bname
  let binary_desc b = b.TestCases.bdesc
  let binary_arbitrary b = b.TestCases.barbitrary
  let binary_fad b = b.TestCases.bfad
  let binary_f b = b.TestCases.bf
  let binary_dfdx b = b.TestCases.bdfdx
  let binary_dfdy b = b.TestCases.bdfdy

  let unary_tests = TestCases.unary
  let binary_tests = TestCases.binary

  let compute_unary t v =
    let x = Op.make v in
    Op.diff x 0 1;
    let v_fad = (unary_fad t) x in
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (unary_f t) v in
    (* derivative of f *)
    let dfdx_fad = Op.d v_fad 0 in
    let actual_dfdx = (unary_dfdx t) v in
    ((vf, actual_vf), (dfdx_fad, actual_dfdx))

  let compute_binary t (v1, v2) =
    let x = Op.make v1 in
    Op.diff x 0 2;
    let y = Op.make v2 in
    Op.diff y 1 2;
    let v_fad = (binary_fad t) x y in
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (binary_f t) v1 v2 in
    (* derivative of f *)
    let dfdx_fad = Op.d v_fad 0 in
    let actual_dfdx = (binary_dfdx t) v1 v2 in
    let dfdy_fad = Op.d v_fad 1 in
    let actual_dfdy = (binary_dfdy t) v1 v2 in
    ((vf, actual_vf), (dfdx_fad, actual_dfdx), (dfdy_fad, actual_dfdy))
end

module ComputeBAD =
struct
  module Op = B(OpFloat)
  module TestCases = Test_cases.Make(Op)

  type unary = TestCases.unary_test
  type binary = TestCases.binary_test
  type value = Op.t

  let unary_name u = u.TestCases.uname
  let unary_desc u = u.TestCases.udesc
  let unary_arbitrary u = u.TestCases.uarbitrary
  let unary_fad u = u.TestCases.ufad
  let unary_f u = u.TestCases.uf
  let unary_dfdx u = u.TestCases.udfdx

  let binary_name b = b.TestCases.bname
  let binary_desc b = b.TestCases.bdesc
  let binary_arbitrary b = b.TestCases.barbitrary
  let binary_fad b = b.TestCases.bfad
  let binary_f b = b.TestCases.bf
  let binary_dfdx b = b.TestCases.bdfdx
  let binary_dfdy b = b.TestCases.bdfdy

  let unary_tests = TestCases.unary
  let binary_tests = TestCases.binary

  let compute_unary t v =
    let x = Op.make v in
    let v_fad = (unary_fad t) x in
    Op.diff v_fad 0 1;
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (unary_f t) v in
    (* derivative of f *)
    let dfdx_fad = Op.d x 0 in
    let actual_dfdx = (unary_dfdx t) v in
    ((vf, actual_vf), (dfdx_fad, actual_dfdx))

  let compute_binary t (v1, v2) =
    let x = Op.make v1 in
    let y = Op.make v2 in
    let v_fad = (binary_fad t) x y in
    Op.diff v_fad 0 1;
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (binary_f t) v1 v2 in
    (* derivative of f *)
    let dfdx_fad = Op.d x 0 in
    let actual_dfdx = (binary_dfdx t) v1 v2 in
    let dfdy_fad = Op.d y 0 in
    let actual_dfdy = (binary_dfdy t) v1 v2 in
    ((vf, actual_vf), (dfdx_fad, actual_dfdx), (dfdy_fad, actual_dfdy))
end

module Test(Compute : Compute) =
struct
  let compare_unary t v =
    let ((vf, actual_vf), (dfdx_fad, actual_dfdx)) = Compute.compute_unary t v in
    (eq_float vf actual_vf) && (eq_float dfdx_fad actual_dfdx)

  let compare_binary t (v1, v2) =
    let ((vf, actual_vf), (dfdx_fad, actual_dfdx), (dfdy_fad, actual_dfdy)) =
      Compute.compute_binary t (v1, v2) in
    (eq_float vf actual_vf) && (eq_float dfdx_fad actual_dfdx) &&
    (eq_float dfdy_fad actual_dfdy)

  let test_unary ?count:(count=100) f =
    let cell = QCheck.(Test.make_cell ~name:(Compute.unary_name f)
                         ~count:count (Compute.unary_arbitrary f)
                         (compare_unary f)) in
    cell, QCheck.Test.check_cell cell

  let test_binary ?count:(count=100) f =
    let cell = QCheck.(Test.make_cell ~name:(Compute.binary_name f)
                         ~count:count (Compute.binary_arbitrary f)
                         (compare_binary f)) in
    cell, QCheck.Test.check_cell cell

  let show_test_unary t v =
    let ((vf, actual_vf), (dfdx_fad, actual_dfdx)) =
      Compute.compute_unary t v in
    Printf.printf "\tDESC : %s\n" (Compute.unary_desc t);
    Printf.printf "\tx = %f\n" v;
    Printf.printf "\tf    : expected %f, got %f\n" vf actual_vf;
    Printf.printf "\tdfdx : expected %f, got %f\n" dfdx_fad actual_dfdx

  let show_test_binary t (v1, v2) =
    let ((vf, actual_vf), (dfdx_fad, actual_dfdx), (dfdy_fad, actual_dfdy)) =
      Compute.compute_binary t (v1, v2) in
    Printf.printf "\tDESC : %s\n" (Compute.binary_desc t);
    Printf.printf "\tx, y = %e, %e\n" v1 v2;
    Printf.printf "\tf    : expected %e, got %e ... %s\n" actual_vf vf
      (string_of_bool (eq_float vf actual_vf));
    Printf.printf "\tdfdx : expected %e, got %e ... %s\n" actual_dfdx dfdx_fad
      (string_of_bool (eq_float dfdx_fad actual_dfdx));
    Printf.printf "\tdfdy : expected %e, got %e ... %s\n" actual_dfdy dfdy_fad
      (string_of_bool (eq_float dfdy_fad actual_dfdy));
    ()

  let show_result show_test test (cell, res) =
    begin match res.QCheck.TestResult.state with
    | Success ->
      Printf.printf "%s\t:\tOK\n" (QCheck.Test.get_name cell)
    | Failed { instances = instance :: _ }
    | Error { instance } ->
      let name = QCheck.Test.get_name cell in
      let arb = QCheck.Test.get_arbitrary cell in
      let c_ex = instance.instance in
      Printf.printf "%s\t:\tNOT OK\n" name;
      Printf.printf "\tCounter-example: %s\n"
        (QCheck.Test.print_c_ex arb instance);
      show_test test c_ex
    | Failed { instances = [] } ->
      Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
    | Failed_other { msg } ->
      Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
      Printf.printf "\tMessage: %s\n" msg
    end; flush stdout

  let test_unary_arr ?count:(count=100) tests =
    let results =
      Array.map (fun t -> test_unary ~count:count t)
        tests in
    let ok = check_results results in
    Array.iter2 (show_result show_test_unary) tests results;
    print_endline (if ok then "OK" else "NOT OK");
    ok

  let test_binary_arr ?count:(count=100) tests =
    let results =
      Array.map (fun t -> test_binary ~count:count t)
        tests in
    let ok = check_results results in
    Array.iter2 (show_result show_test_binary) tests results;
    print_endline (if ok then "OK" else "NOT OK");
    ok

  let test_all () =
    Random.self_init ();

    print_endline "---- UNARY FUNCTIONS";
    ignore (test_unary_arr ~count:1000 Compute.unary_tests);

    print_endline "\n---- BINARY FUNCTIONS";
    ignore (test_binary_arr ~count:1000 Compute.binary_tests);

    ()
end

module TestFAD = Test(ComputeFAD)
module TestBAD = Test(ComputeBAD)

let _ =
  print_endline "TEST FAD";
  TestFAD.test_all ();

  print_newline ();

  print_endline "TEST BAD";
  TestBAD.test_all ();
