open Utils

module type OpS = Op.S

module Derivatives (Op : OpS) =
struct
  type t = Op.t array ref

  let create () = ref [||]
  let copy v = Array.copy !v

  let get v i = !v.(i)

  let length v = Array.length !v
  let have_values v = length v > 0

  let diff v i n =
    user_assert (i < n)
      ("Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int n) ^ "]");
    let res = if have_values v then v else ref (Array.make n (Op.zero ())) in
    !res.(i) <- Op.one ();
    res

  let add v v' =
    user_assert (have_values v') "Propagating node with no derivatives";
    if have_values v then begin
      user_assert (length v = length v')
        ("Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      Array.iteri (fun i _ -> ignore Op.(!v.(i) += !v'.(i))) !v;
    end else v := copy v'

end

module BTypeNameHV (Op : OpS) =
struct

end
