module Interval = Interval
module AffineForm = AffineForm

let to_affine_form (i : Interval.t) =
  AffineForm.make_bounds i.min i.max

let to_interval aaf =
  let radius = AffineForm.radius aaf in
  let center = aaf.t_center in
  Interval.make_bounds (center -. radius) (center +. radius)
