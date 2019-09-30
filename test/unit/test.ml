open Fadbad

module Op = Op.OpFloat
module F = F(Op)

type unary_test = {
  uname : string;
  uarbitrary : float QCheck.arbitrary;
  ufad : F.t -> F.t;
  uf : float -> float;
  udfdx : float -> float;
}

type binary_test = {
  bname : string;
  barbitrary : (float * float) QCheck.arbitrary;
  bfad : F.t -> F.t -> F.t;
  bf : float -> float -> float;
  bdfdx : float -> float -> float;
  bdfdy : float -> float -> float;
}

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

let show_test t v =
  let x = F.make v in
  F.diff x 0 1;

  let v_fad = t.ufad x in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = t.uf v in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = t.udfdx v in

  Printf.printf "\tv = %f\n" v;
  Printf.printf "\tf    : expected %f, got %f\n" vf actual_vf;
  Printf.printf "\tdfdx : expected %f, got %f\n" dfdx_fad actual_dfdx

let show_test2 t (v1, v2) =
  let x = F.make v1 in
  F.diff x 0 2;
  let y = F.make v2 in
  F.diff y 1 2;

  let v_fad = t.bfad x y in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = t.bf v1 v2 in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = t.bdfdx v1 v2 in
  let dfdy_fad = F.d v_fad 1 in
  let actual_dfdy = t.bdfdy v1 v2 in

  Printf.printf "\tv = %e, %e\n" v1 v2;
  Printf.printf "\tf    : expected %e, got %e ... %s\n" vf actual_vf
    (string_of_bool (eq_float vf actual_vf));
  Printf.printf "\tdfdx : expected %e, got %e ... %s\n" dfdx_fad actual_dfdx
    (string_of_bool (eq_float dfdx_fad actual_dfdx));
  Printf.printf "\tdfdy : expected %e, got %e ... %s\n" dfdy_fad actual_dfdy
    (string_of_bool (eq_float dfdy_fad actual_dfdy));
  ()

let show_result test (cell, res) =
  match res.QCheck.TestResult.state with
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

let show_result2 test (cell, res) =
  match res.QCheck.TestResult.state with
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
    show_test2 test c_ex
  | Failed { instances = [] } ->
    Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
  | Failed_other { msg } ->
    Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
    Printf.printf "\tMessage: %s\n" msg

let check_results results =
  Array.for_all (fun (cell, res) ->
      match res.QCheck.TestResult.state with
      | Success -> true
      | Error _ | Failed _ | Failed_other _ -> false
    ) results

(* main test functions *)

let compare t v =
  let x = F.make v in
  F.diff x 0 1;

  let v_fad = t.ufad x in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = t.uf v in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = t.udfdx v in

  (eq_float vf actual_vf) &&
  (eq_float dfdx_fad actual_dfdx)

let test ?count:(count=100) f =
  let cell = QCheck.(Test.make_cell ~name:f.uname ~count:count f.uarbitrary
                    (compare f)) in
  cell, QCheck.Test.check_cell cell

let test_arr ?count:(count=100) tests =
  let results =
    Array.map (fun t -> test ~count:count t)
      tests in
  let ok = check_results results in
  Array.iter2 show_result tests results;
  print_endline (if ok then "OK" else "NOT OK");
  ok

let compare2 t (v1, v2) =
  let x = F.make v1 in
  F.diff x 0 2;
  let y = F.make v2 in
  F.diff y 1 2;

  let v_fad = t.bfad x y in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = t.bf v1 v2 in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = t.bdfdx v1 v2 in
  let dfdy_fad = F.d v_fad 1 in
  let actual_dfdy = t.bdfdy v1 v2 in

  (eq_float vf actual_vf) &&
  (eq_float dfdx_fad actual_dfdx) &&
  (eq_float dfdy_fad actual_dfdy)

let test2 ?count:(count=100) f =
  let cell = QCheck.(Test.make_cell ~name:f.bname ~count:count f.barbitrary
                       (compare2 f)) in
  cell, QCheck.Test.check_cell cell

let test2_arr ?count:(count=100) tests =
  let results =
    Array.map (fun t -> test2 ~count:count t)
      tests in
  let ok = check_results results in
  Array.iter2 show_result2 tests results;
  print_endline (if ok then "OK" else "NOT OK");
  ok


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
