(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                             Copyright 2019                             *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(** Forward Automatic Differentiation (FAD) *)

open Fadbad_utils

module FTypeName (Op : Types.OpS) =
struct

  type op_t = Op.t

  type t = {
    m_val : op_t;
    mutable m_diff : op_t array;
  }

  type elt = Op.elt
  type scalar = Op.scalar

let string_of_scalar = Op.string_of_scalar
  let string_of_elt = Op.string_of_elt

  let to_string this =
    Printf.sprintf "{%s | [%s]}" (Op.to_string this.m_val)
      (String.concat ", " (Array.to_list (Array.map Op.to_string this.m_diff)))

  let create () = {
    m_val = Op.create ();
    m_diff = Array.make 0 (Op.zero ());
  }

  let get v = Op.get v.m_val
  let ( !! ) = get

  let lift v = { (create ()) with m_val = v; }

  let make v = lift (Op.make v)
  let integer i = lift (Op.integer i)
  let zero () = lift (Op.zero ())
  let one () = lift (Op.one ())
  let two () = lift (Op.two ())

  let scale x a = {
    m_val = Op.scale x.m_val a;
    m_diff = Array.map (fun x -> Op.scale x a) x.m_diff;
  }

  let translate x a = {
    m_val = Op.translate x.m_val a;
    m_diff = Array.map Op.copy x.m_diff;
  }

  let copy v = {
    m_val = Op.copy v.m_val;
    m_diff = Array.map Op.copy v.m_diff;
  }

  let deepcopy v = {
    m_val = Op.deepcopy v.m_val;
    m_diff = Array.map Op.deepcopy v.m_diff;
  }

  let dim v = Array.length v.m_diff
  let value v = v.m_val

  (** [deriv f i] retrieves the derivative of variable of index [i] in
      computation [f] *)
  let deriv v i =
    if i < (dim v) then v.m_diff.(i)
    else Op.zero ()

  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt] *)
  let d v i = Op.get (deriv v i)

  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)
  let diff v idx n =
    user_assert (idx < n && idx >= 0)
      ("Index " ^ (string_of_int idx) ^
       " out of bounds [0," ^ (string_of_int n) ^ "]");

    if (dim v) = 0 then begin
      v.m_diff <- Array.init n (fun _ -> Op.zero ());
    end else
      user_assert ((dim v) = n) "derivative vectors not of same dim";

    Array.fill v.m_diff 0 (dim v) (Op.zero ());
    v.m_diff.(idx) <- Op.one ()

  (**/**)
  let depend v = (dim v) <> 0

  let setDepend v v' =
    internal_assert ((dim v') > 0) "input is not a dependent variable";
    if ((dim v) = 0) then begin
      v.m_diff <- Array.init (dim v') (fun _ -> Op.zero ());
    end else
      user_assert ((dim v) = (dim v'))
        ("derivative vectors not of the same dim "
         ^ (string_of_int (dim v)) ^ "," ^ (string_of_int (dim v')))

  let setDepend2 v v1 v2 =
    internal_assert ((dim v1) = (dim v2))
      ("derivative vectors not of same dim "
       ^ (string_of_int (dim v1)) ^ "," ^ (string_of_int (dim v2)));
    internal_assert ((dim v1) > 0) "lhs-input is not a dependent variable";
    internal_assert ((dim v2) > 0) "rhs-input is not a dependent variable";
    if ((dim v) = 0) then begin
      v.m_diff <- Array.init (dim v1) (fun _ -> Op.zero ());
    end else
      user_assert ((dim v) = (dim v1))
        ("derivative vectors not of the same dim "
         ^ (string_of_int (dim v)) ^ "," ^ (string_of_int (dim v1)))
  (**/**)

  (* ------------------------------ *)
  (* COMPARISON OPERATORS *)
  (* ------------------------------ *)

  let ( = ) v v' = Op.( v.m_val = v'.m_val )
  let ( <> ) v v' = Op.( v.m_val <> v'.m_val )

  (* ------------------------------ *)
  (* ARITHMETIC OPERATORS *)
  (* ------------------------------ *)

  (* ADD *)

  (**/**)
  let addV (v : t) (v' : Op.t) : t =
    let res = lift Op.(v.m_val + v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (dim v);
    res

  let vAdd v v' =
    let res = lift Op.(v + v'.m_val) in
    setDepend res v';
    Array.blit v'.m_diff 0 res.m_diff 0 (dim v');
    res

  let add v v' =
    let res = lift Op.(v.m_val + v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(v.m_diff.(i) + v'.m_diff.(i)))
      res.m_diff;
    res
  (**/**)

  let ( + ) v v' =
    match depend v, depend v' with
    | false, false -> lift Op.(v.m_val + v'.m_val)
    | true, false -> addV v v'.m_val
    | false, true -> vAdd v.m_val v'
    | true, true -> add v v'

  let ( += ) v v' =
    ignore Op.(v.m_val += v'.m_val);
    if not (depend v') then v
    else begin
      if depend v then
        Array.iteri (fun i vi -> ignore Op.(vi += v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.blit v'.m_diff 0 v.m_diff 0 (dim v)
      end;
      v
    end

  (* SUB *)

  (**/**)
  let subV v v' =
    let res = lift Op.(v.m_val - v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (dim v);
    res

  let vSub v v' =
    let res = lift Op.(v - v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.( - v'.m_diff.(i) ))
      res.m_diff;
    res

  let sub v v' =
    let res = lift Op.(v.m_val - v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.( v.m_diff.(i) - v'.m_diff.(i) ))
      res.m_diff;
    res
  (**/**)

  let ( - ) v v' =
    match depend v, depend v' with
    | false, false -> lift Op.(v.m_val - v'.m_val)
    | true, false -> subV v v'.m_val
    | false, true -> vSub v.m_val v'
    | true, true -> sub v v'

  let ( -= ) v v' =
    ignore Op.(v.m_val -= v'.m_val);
    if depend v' then begin
      if depend v then
        Array.iteri (fun i vi -> ignore Op.(vi -= v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.iteri (fun i _ -> v.m_diff.(i) <- Op.(-v'.m_diff.(i))) v.m_diff
      end;
    end;
    v

  (* MUL *)

  (**/**)
  let mulV v v' =
    let res = lift Op.(v.m_val * v') in
    setDepend res v;
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * v'))
      res.m_diff;
    res

  let vMul v v' =
    let res = lift Op.(v * v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v'.m_diff.(i) * v))
      res.m_diff;
    res

  let mul v v' =
    let res = lift Op.(v.m_val * v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          Op.((v.m_diff.(i) * v'.m_val) + (v'.m_diff.(i) * v.m_val))
      )
      res.m_diff;
    res
  (**/**)

  let ( * ) v v' =
    match depend v, depend v' with
    | false, false -> lift Op.(v.m_val * v'.m_val)
    | true, false -> mulV v v'.m_val
    | false, true -> vMul v.m_val v'
    | true, true -> mul v v'

  let ( *= ) v v' =
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ -> v.m_diff.(i) <-
                      Op.((v.m_diff.(i) * v'.m_val) + (v'.m_diff.(i) * v.m_val))
                  )
        v.m_diff
    | true, _ -> Array.iteri (fun i vi -> ignore Op.(vi *= v'.m_val))
                   v.m_diff
    | _ -> (* _, true *)
      setDepend v v';
      Array.iteri (fun i _ -> v.m_diff.(i) <- Op.( v'.m_diff.(i) * v.m_val ))
        v.m_diff;
    end;
    ignore Op.(v.m_val *= v'.m_val);
    v

  (* DIV *)

  (**/**)
  let divV v v' =
    let cval = Op.(v.m_val / v') in
    let res = lift cval in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / v'))
        res.m_diff
    end;
    res

  let vDiv v v' =
    let cval = Op.(v / v'.m_val) in
    let res = lift cval in
    if depend v' then begin
      let tmp = Op.(- res.m_val / v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i)))
        res.m_diff
    end;
    res

  let div v v' =
    let cval = Op.(v.m_val / v'.m_val) in
    let res = lift cval in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          Op.((v.m_diff.(i) - (cval * v'.m_diff.(i))) / v'.m_val)
      ) res.m_diff;
    res
  (**/**)

  let ( / ) v v' =
    match depend v, depend v' with
    | false, false -> lift Op.(v.m_val / v'.m_val)
    | true, false -> divV v v'.m_val
    | false, true -> vDiv v.m_val v'
    | true, true -> div v v'

  let ( /= ) v v' =
    ignore Op.(v.m_val /= v'.m_val);
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            Op.((v.m_diff.(i) - v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff
    | true, _ -> Array.iteri (fun i _ -> ignore Op.(v.m_diff.(i) /= v'.m_val))
                   v.m_diff
    | _ -> (* _, true *)
      setDepend v v';
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            Op.(- (v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff;
    end;
    v

  (* POW *)

  (**/**)
  let powV v v' =
    let res = lift Op.(v.m_val ** v') in
    let tmp = Op.(v' * (v.m_val ** (v' - (one ())))) in
    setDepend res v;
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(tmp * v.m_diff.(i))
      ) v.m_diff;
    res

  let vPow v v' =
    let res = lift Op.(v ** v'.m_val) in
    let tmp = Op.(res.m_val * (log v)) in
    setDepend res v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i))
      ) v'.m_diff;
    res

  let pow v v' =
    let res = lift Op.(v.m_val ** v'.m_val) in
    let tmp1 = Op.(v'.m_val * (v.m_val ** (v'.m_val - (one ())))) in
    let tmp2 = Op.(res.m_val * (log v.m_val)) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.((tmp1 * v.m_diff.(i)) + (tmp2 * v'.m_diff.(i)))
      ) v.m_diff;
    res
  (**/**)

  let ( ** ) v v' =
    match depend v, depend v' with
    | false, false -> lift Op.(v.m_val ** v'.m_val)
    | true, false -> powV v v'.m_val
    | false, true -> vPow v.m_val v'
    | true, true -> pow v v'

  (* ------------------------------ *)
  (* UNARY FUNCTIONS *)
  (* ------------------------------ *)

  let ( ~+ ) v =
    let res = lift Op.(+v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(+v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let ( ~- ) v =
    let res = lift Op.(- v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(-v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let sqr v =
    let res = lift (Op.sqr v.m_val) in
    if depend v then begin
      let tmp = Op.((two ()) * v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let inv v =
    let res = lift Op.(inv v.m_val) in
    if depend v then begin
      let tmp = Op.(- inv (sqr v.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let exp v =
    let res = lift Op.(exp v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * res.m_val))
        res.m_diff;
    end;
    res

  let log v =
    let res = lift Op.(log v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / v.m_val))
        res.m_diff;
    end;
    res

  let sqrt v =
    let res = lift Op.(sqrt v.m_val) in
    if depend v then begin
      let tmp = Op.((two ()) * res.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / tmp))
        res.m_diff;
    end;
    res

  let sin v =
    let res = lift Op.(sin v.m_val) in
    if depend v then begin
      let tmp = Op.cos v.m_val in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let cos v =
    let res = lift Op.(cos v.m_val) in
    if depend v then begin
      let tmp = Op.(- sin v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let tan v =
    let res = lift Op.(tan v.m_val) in
    if depend v then begin
      let tmp = Op.((one ()) + (sqr res.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let asin v =
    let res = lift Op.(asin v.m_val) in
    if depend v then begin
      let tmp = Op.(inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let acos v =
    let res = lift Op.(acos v.m_val) in
    if depend v then begin
      let tmp = Op.(- inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let atan v =
    let res = lift Op.(atan v.m_val) in
    if depend v then begin
      let tmp = Op.(inv ((one ()) + (sqr v.m_val))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res
end

module OrderedFTypeName(Op : Types.OrderedOpS) =
struct
  include FTypeName(Op)

  let ( < ) a b = Op.(value a < value b)
  let ( <= ) a b = Op.(value a <= value b)
  let ( > ) a b = Op.(value a > value b)
  let ( >= ) a b = Op.(value a >= value b)

  let min a b = if a < b then a else b
  let max a b = if a > b then a else b
end
