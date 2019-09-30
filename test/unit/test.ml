open Fadbad

(* utils *)

let eq_float a b epsilon =
  let absA = abs_float a in
  let absB = abs_float b in
  let diff = abs_float (a -. b) in

  if a = b then
    (* shortcut, handles infinities *)
    true
  else if a = 0. || b = 0. || (absA +. absB < epsilon_float) then
    (* a or b is zero or both are extremely close to it
       relative error is less meaningful here *)
    diff < (epsilon *. epsilon_float)
  else
    diff /. (min (absA +. absB) max_float) < epsilon

let show_result (cell, res) =
  match res.QCheck.TestResult.state with
  | Success ->
    Printf.printf "%s\t:\tOK\n" (QCheck.Test.get_name cell)
  | Error { instance } ->
    Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
    Printf.printf "\tCounter-example: %s\n"
      (QCheck.Test.print_c_ex (QCheck.Test.get_arbitrary cell) instance)
  | Failed { instances } ->
    Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
    Printf.printf "\tCounter-example: %s (1 of %d)\n"
      (QCheck.Test.print_c_ex (QCheck.Test.get_arbitrary cell)
         (List.hd instances))
      (List.length instances)
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

module Op = Op.OpFloat
module F = F(Op)

let compare
    (fun_fad : F.t -> F.t)
    (f : float -> float)
    (dfdx : float -> float)
    (v : float)
  =

  let x = F.make v in
  F.diff x 0 1;

  let v_fad = fun_fad x in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = f v in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = dfdx v in

  (eq_float vf actual_vf 1e-10) && (eq_float dfdx_fad actual_dfdx 1e-10)

let test_unary ?count:(count=100) f =
  let (name, arbitrary, f) = f in
  let cell = QCheck.(Test.make_cell ~name:name ~count:count arbitrary f) in
  cell, QCheck.Test.check_cell cell

let test_unary_arr ?count:(count=100) tests =
  let results =
    Array.map (fun t -> test_unary ~count:count t)
      tests in

  let ok = check_results results in
  Array.iter show_result results;

  print_endline (if ok then "OK" else "NOT OK");
  ok

let compare2
    (fun_fad : F.t -> F.t -> F.t)
    (f : float -> float -> float)
    (dfdx : float -> float -> float)
    (dfdy : float -> float -> float)
    ((v1, v2) : float * float) =

  let x = F.make v1 in
  F.diff x 0 2;
  let y = F.make v2 in
  F.diff y 1 2;

  let v_fad = fun_fad x y in

  (* value of f *)
  let vf = F.get v_fad in
  let actual_vf = f v1 v2 in

  (* derivative of f *)
  let dfdx_fad = F.d v_fad 0 in
  let actual_dfdx = dfdx v1 v2 in
  let dfdy_fad = F.d v_fad 1 in
  let actual_dfdy = dfdy v1 v2 in

  (eq_float vf actual_vf 1e-10) &&
  (eq_float dfdx_fad actual_dfdx 1e-10) &&
  (eq_float dfdy_fad actual_dfdy 1e-10)

let test_binary ?count:(count=100) f =
  let (name, arbitrary, f) = f in
  let cell = QCheck.(Test.make_cell ~name:name ~count:count arbitrary f) in
  cell, QCheck.Test.check_cell cell

let test_binary_arr ?count:(count=100) tests =
  let results =
    Array.map (fun t -> test_binary ~count:count t)
      tests in

  let ok = check_results results in
  Array.iter show_result results;
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
