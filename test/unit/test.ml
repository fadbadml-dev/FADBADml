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

(* main test functions *)

module Op = Op.OpFloat
module F = F(Op)

let compare
    (fun_fad : F.t -> F.t)
    (fun_x : float -> float)
    (fun_xp : float -> float)
    (v : float)
  =

  let x = F.make (Op.make v) in
  F.diff x 0 1;

  let f = fun_fad x in

  (* value of f *)
  let vf = Op.get (F.value f) in
  let vfx = fun_x v in

  (* derivative of f *)
  let df = Op.get (F.d f 0) in
  let dfx = fun_xp v in

  (eq_float vf vfx 1e-10) && (eq_float df dfx 1e-10)

let test ?count:(count=100) f =
  let (name, arbitrary, f) = f in
  let cell = QCheck.(Test.make_cell ~name:name ~count:count arbitrary f) in
  QCheck.Test.check_cell cell

let test_all ?count:(count=100) tests =
  let results =
    Array.map (fun ((name, _, _) as t) -> name, test ~count:count t)
      tests in

  Array.iter (fun (name, res) ->
      let str_res =
        if QCheck.TestResult.is_success res then "OK" else "NOT OK"
      in
      Printf.printf "%s: %s\n" name str_res
    ) results

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
