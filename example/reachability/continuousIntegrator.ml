module Make (Func : Fode.S) (Op : Sets.S) = struct
  module FuncInterval = Func(Interval)
  module Func = Func(Op)

  let dt = ref 1e-1
  let order = ref 3

  let margin = 0.1
  let dilation = 1.1

  let for_all2 p s1 s2 =
    let dim = Array.length s1 in
    let rec test i =
      if i >= dim then
        true
      else if p s1.(i) s2.(i) then
        test (i+1)
      else
        false
    in
    test 0


  let add_margin s m =
    let open Interval in
    let margin  = make_bounds (-.m) m in
    Array.map (fun i -> i + margin) s

  let dilate s a =
    let open Interval in
    Array.map
      (fun i ->
        let rad = radius i in
        let margin = make_bounds (-.rad) rad in
        let margin = scale margin a in
        i + margin)
      s

  let scale s a =
    let open Interval in
    Array.map (fun i ->  i * a) s

  let add s1 s2 =
    let open Interval in
    Array.map2 (fun i1 i2 -> i1 + i2) s1 s2

  let compute_enclosure s0 t0 =
    let s0 = Array.map
               (fun s ->
                 let min,max = Op.get_min_max s in
                 Interval.make_bounds min max)
               s0
    in
    let deltaT = Interval.make_bounds 0. !dt in
    let compute_next enc =
      add s0 (scale (FuncInterval.exec enc deltaT) deltaT)
    in
    let rec find_fix_point enc next =
      if for_all2 Interval.subset next enc then
        next
      else
        let enc = dilate enc dilation in
        let next = compute_next enc in
        find_fix_point enc next
    in
    let enc = add_margin s0 0.1 in (* ensure no punctual state *)
    let next_enc = compute_next enc in
    find_fix_point enc next_enc
end
