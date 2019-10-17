open Sets

type t =
  {
    mutable dependent: AffineForm.t list; (* first coefficients *)
    mutable independent : Interval.t list; (* remaind coefficients *)
  }

let create () =
  {
    dependent = [];
    independent = [];
  }

let add_dependent tm aaf =
  let () =
    Utils.user_assert
      (tm.independent = [])
      "Cannot add dependent coefficient because some independent ones are already defined"
  in
  let () = tm.dependent <- (aaf :: tm.dependent) in
  tm

let add_independent tm i =
  let () = tm.independent <- (i :: tm.independent) in
  ()

let eval tm dt =
  let eval_dep l =
    let open AffineForm in
    let rec eval_dep r l =
      match l with
      | [] -> r
      | h::t -> eval_dep ((dt * r) + h) t
    in
    eval_dep (zero ()) l
  in
  let eval_ind l i0 =
    let dt = to_interval dt in
    let open Interval in
    let rec eval_ind r l powdt =
      match l with
      | [] -> r
      | h::t -> eval_ind (r + (h * powdt)) t (powdt * dt)
    in
    eval_ind (zero ()) l (pow_int dt i0)
  in
  let dep = eval_dep tm.dependent in
  match tm.independent with
  | [] -> dep
  | l ->
     AffineForm.(dep + (to_affine_form (eval_ind l (List.length tm.dependent))))
