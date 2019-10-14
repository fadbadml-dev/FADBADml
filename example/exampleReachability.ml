module Interval =
  struct
    type scalar = float
    type elt =
      {
        min: scalar;
        max: scalar;
      }

    type t =
      {
        mutable min: scalar;
        mutable max: scalar;
      }

    let create () = { min = Float.nan; max = Float.nan; }

    let make_point f = { min = f; max = f; }
    let make (i : elt) : t = {
        min = i.min;
        max = i.max;
      }

    let integer i = make_point (float i)

    let get (i : t) : elt =
      {
        min = i.min;
        max = i.max;
      }

    let to_string i =
      Printf.sprintf "[%f,%f]" i.min i.max
    let string_of_scalar = string_of_float

    let copy i = {
        min = i.min;
        max = i.max;
      }
    let deepcopy = copy

    let zero () = integer 0
    let one () = integer 1
    let two () = integer 2

    let scale i f =
      make { min = i.min *. f; max = i.max *. f; }

    let translate i f =
      make { min = i.min +. f; max = i.max +. f; }

    let ( ~+ ) = copy
    let ( ~- ) i = make { min = -. i.max; max = -. i.min; }

    let ( + ) i1 i2 = {
        min = i1.min +. i2.min;
        max = i1.max +. i2.max;
      }

    let ( += ) i1 i2 =
      let () = i1.min <- i1.min +. i2.min in
      let () = i1.max <- i1.max +. i2.max in
      i1

    let ( - ) i1 i2 = {
        min = i1.min -. i2.min;
        max = i1.max -. i2.max;
      }

    let ( -= ) i1 i2 =
      let () = i1.min <- i1.min -. i2.min in
      let () = i1.max <- i1.max -. i2.max in
      i1

    let ( * ) i1 i2 =
      let a = i1.min *. i2.min in
      let b = i1.min *. i2.max in
      let c = i1.max *. i2.min in
      let d = i1.max *. i2.max in
      {
        min = min (min a b) (min c d);
        max = max (max a b) (max c d);
      }

    let ( *= ) i1 i2 =
      let i = i1 * i2 in
      let () = i1.min <- i.min in
      let () = i1.max <- i.max in
      i1

    let inv i =
      match i.min = 0., i.max = 0. with
      | true, true -> begin
          Printf.eprintf "Interval: zero division";
          exit 1
        end
      | true, false -> {
          min = 1. /. i.max;
          max = Float.infinity;
        }
      | false, true -> {
          min = Float.neg_infinity;
          max = 1. /. i.min;
        }
      | false, false -> begin
          if i.min <= 0. && 0. <= i.max then
            {
              min = Float.neg_infinity;
              max = Float.infinity;
            }
          else
            {
              min = 1. /. i.max;
              max = 1. /. i.min;
            }
        end

    let ( / ) i1 i2 = i1 * (inv i2)
    let ( /= ) i1 i2 =
      let i = i1 / i2 in
      let () = i1.min <- i.min in
      let () = i1.max <- i.max in
      i1

    let sqr i =
      if i.max <= 0. then
        {
          min = i.max *. i.max;
          max = i.min *. i.min;
        }
      else if 0. <= i.min then
        {
          min = i.min *. i.min;
          max = i.max *. i.max;
        }
      else (* i.min < 0. && 0. < i.max *)
        {
          min = 0.;
          max = max (i.min *. i.min) (i.max *. i.max);
        }

    let sqrt i =
      {
        min = sqrt i.min;
        max = sqrt i.max;
      }

    let log i =
      {
        min = log i.min;
        max = log i.max;
      }

    let exp i =
      {
        min = exp i.min;
        max = exp i.max;
      }

    let ( ** ) i1 i2 = exp (i2 * (log i1))

    let sin i =
      assert false (* TODO *)

    let cos i =
      assert false (* TODO *)

    let tan i =
      assert false (* TODO *)

    let asin i =
      assert false (* TODO *)

    let acos i =
      assert false (* TODO *)

    let atan i =
      assert false (* TODO *)

    let ( = ) i1 i2 =
      i1.min = i2.min && i1.max = i2.max

    let ( <> ) i1 i2 =
      i1.min <> i2.min || i1.max <> i2.max

    let ( < ) i1 i2 =
      i1.max < i2.min

    let ( <= ) i1 i2 =
      i1.max <= i2.min

    let ( > ) i1 i2 =
      i1.min > i2.max

    let ( >= ) i1 i2 =
      i1.min >= i2.max

    (** [subset s1 s2] tests whether the set s1 is a subset of the set s2. *)
    let subset i1 i2 =
      Stdlib.(i2.min <= i1.min && i1.max <= i2.max)
  end

module F = Fadbad.F(Interval)

exception Picard_not_contract
let rec picard x0 f dt x nb_max =
  if nb_max <= 0 then
    x
  else
    let next_x = F.(x0 + ((f x) * dt)) in
    if Interval.subset (F.value next_x) (F.value x) then
      picard x0 f dt next_x (nb_max - 1)
    else
      raise Picard_not_contract


let () =
  let open Interval in
  let i = make {min = 2.3; max = 5.2; } in
  let () = print_endline (to_string i) in
  ()
