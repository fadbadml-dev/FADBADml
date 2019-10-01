open Utils

module type OpS = Op.S

module FTypeName (Op : OpS) =
struct

  type t = {
    m_val : Op.t;
    mutable m_diff : Op.t array;
  }

  type elt = Op.elt
  type scalar = Op.scalar

  let create () = {
    m_val = Op.zero ();
    m_diff = Array.make 0 (Op.zero ());
  }

  let make v = { (create ()) with m_val = Op.make v; }
  let get v = Op.get v.m_val

  let lift v = { (create ()) with m_val = v; }

  let zero () = lift (Op.zero ())
  let one () = lift (Op.one ())
  let two () = lift (Op.two ())

  let scalar_one = Op.scalar_one

  let copy v = {
    m_val = Op.copy v.m_val;
    m_diff = Array.map Op.copy v.m_diff;
  }

  let length v = Array.length v.m_diff
  let value v = v.m_val

  (** [deriv f i] retrieves the derivative of variable of index [i] in
      computation [f] *)
  let deriv v i =
    if i < (length v) then v.m_diff.(i)
    else Op.zero ()

  (** [d_n f \[i1;...;in\]] returns the value of df/dx{_i1}...dx{_in} *)
  let d_n v i_l =
    user_assert (i_l <> []) "d_n : got empty list";
    Op.d_n (v.m_diff.(List.hd i_l)) (List.tl i_l)

  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt] *)
  let d v i = Op.get (deriv v i)

  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)
  let diff v idx n =
    user_assert (idx < n)
      ("Index " ^ (string_of_int idx) ^
       " out of bounds [0," ^ (string_of_int n) ^ "]");

    if (length v) = 0 then begin
      v.m_diff <- Array.make n (Op.zero ());
    end else
      user_assert ((length v) = n) "derivative vectors not of same length";

    Array.fill v.m_diff 0 (length v) (Op.zero ());
    v.m_diff.(idx) <- Op.one ()

  (** [diff_n x i dim n] assigns [i] as index of variable [x] out of [dim]
      up to depth [n] *)
  let diff_n v idx n d =
    if d > 0 then begin
      diff v idx n;
      Op.diff_n (value v) idx n (d-1)
    end

  (**/**)
  let depend v = (length v) <> 0

  let setDepend v v' =
    internal_assert ((length v') > 0) "input is not a dependent variable";
    if ((length v) = 0) then begin
      v.m_diff <- Array.make (length v') (Op.zero ());
    end else
      user_assert ((length v) = (length v'))
        ("derivative vectors not of the same length "
         ^ (string_of_int (length v)) ^ "," ^ (string_of_int (length v')))

  let setDepend2 v v1 v2 =
    internal_assert ((length v1) = (length v2))
      ("derivative vectors not of same length "
       ^ (string_of_int (length v1)) ^ "," ^ (string_of_int (length v2)));
    internal_assert ((length v1) > 0) "lhs-input is not a dependent variable";
    internal_assert ((length v2) > 0) "rhs-input is not a dependent variable";
    if ((length v) = 0) then begin
      v.m_diff <- Array.make (length v1) (Op.zero ());
    end else
      user_assert ((length v) = (length v1))
        ("derivative vectors not of the same length "
         ^ (string_of_int (length v)) ^ "," ^ (string_of_int (length v1)))
  (**/**)

  (* ------------------------------ *)
  (* COMPARISON OPERATORS *)
  (* ------------------------------ *)

  let ( =& ) v v' = Op.( v.m_val = v' )
  let ( &= ) v v' = Op.( v = v'.m_val )
  let ( = ) v v' = Op.( v.m_val = v'.m_val )

  let ( <>& ) v v' = Op.( v.m_val <> v' )
  let ( &<> ) v v' = Op.( v <> v'.m_val )
  let ( <> ) v v' = Op.( v.m_val <> v'.m_val )

  let ( <=& ) v v' = Op.( v.m_val <= v' )
  let ( &<= ) v v' = Op.( v <= v'.m_val )
  let ( <= ) v v' = Op.( v.m_val <= v'.m_val )

  let ( <& ) v v' = Op.( v.m_val < v' )
  let ( &< ) v v' = Op.( v < v'.m_val )
  let ( < ) v v' = Op.( v.m_val < v'.m_val )

  let ( >=& ) v v' = Op.( v.m_val >= v' )
  let ( &>= ) v v' = Op.( v >= v'.m_val )
  let ( >= ) v v' = Op.( v.m_val >= v'.m_val )

  let ( >& ) v v' = Op.( v.m_val > v' )
  let ( &> ) v v' = Op.( v > v'.m_val )
  let ( > ) v v' = Op.( v.m_val > v'.m_val )

  (* ------------------------------ *)
  (* ARITHMETIC OPERATORS *)
  (* ------------------------------ *)

  (* ADD *)

  let ( +& ) (v : t) (v' : scalar) : t =
    let res = lift Op.(v.m_val +& v') in
    if depend v then begin
      setDepend res v;
      Array.blit v.m_diff 0 res.m_diff 0 (length v)
    end; res

  let ( &+ ) v v' =
    let res = lift Op.(v &+ v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.blit v'.m_diff 0 res.m_diff 0 (length v')
    end; res

  (**/**)
  let addV (v : t) (v' : Op.t) : t =
    let res = lift Op.(v.m_val + v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (length v);
    res

  let vAdd v v' =
    let res = lift Op.(v + v'.m_val) in
    setDepend res v';
    Array.blit v'.m_diff 0 res.m_diff 0 (length v');
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
        Array.blit v'.m_diff 0 v.m_diff 0 (length v)
      end;
      v
    end

  let ( +&= ) v v' = ignore Op.(v.m_val +&= v'); v

  (* SUB *)

  let ( -& ) v v' =
    let res = lift Op.(v.m_val -& v') in
    if depend v then begin
      setDepend res v;
      Array.blit v.m_diff 0 res.m_diff 0 (length v)
    end; res

  let ( &- ) v v' =
    let res = lift Op.(v &- v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.( - v'.m_diff.(i) ))
        res.m_diff
    end; res

  (**/**)
  let subV v v' =
    let res = lift Op.(v.m_val - v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (length v);
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

  let ( &-& ) = Op.( &-& )

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

  let ( -&= ) v v' = ignore Op.(v.m_val -&= v'); v

  (* MUL *)

  let ( *& ) v v' =
    let res = lift Op.(v.m_val *& v') in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) *& v'))
        res.m_diff
    end; res

  let ( &* ) v v' =
    let res = lift Op.(v &* v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v &* v'.m_diff.(i)))
        res.m_diff
    end; res

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
    | _ ->
      setDepend v v';
      Array.iteri (fun i _ -> v.m_diff.(i) <- Op.( v'.m_diff.(i) * v.m_val ))
        v.m_diff;
    end;
    ignore Op.( v.m_val *= v'.m_val);
    v

  let ( *&= ) v v' =
    ignore Op.( v.m_val *&= v');
    if depend v then begin
      Array.iteri (fun i vi -> ignore Op.( vi *&= v')) v.m_diff
    end;
    v

  (* DIV *)

  let ( /& ) v v' =
    let cval = Op.(v.m_val /& v') in
    let res = lift cval in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) /& v'))
        res.m_diff
    end;
    res

  let ( &/ ) v v' =
    let cval = Op.(v &/ v'.m_val) in
    let res = lift cval in
    if depend v' then begin
      let tmp = Op.(- res.m_val / v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i)))
        res.m_diff
    end;
    res

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
            Op.(v.m_diff.(i) - (v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff
    | true, _ -> Array.iteri (fun i vi -> ignore Op.(vi /= v'.m_val))
                   v.m_diff
    | _ ->
      setDepend v v';
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            Op.(- (v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff;
    end;
    v

  let ( /&= ) v v' =
    ignore Op.(v.m_val /&= v');
    if depend v then begin
      Array.iteri (fun i vi -> ignore Op.(vi /&= v')) v.m_diff
    end;
    v

  (* POW *)

  let ( **& ) v v' =
    let res = lift Op.(v.m_val **& v') in
    if depend v then begin
      let tmp = Op.(v' &* (v.m_val **& (v' &-& scalar_one))) in
      setDepend res v;
      Array.iteri (fun i _ ->
          res.m_diff.(i) <- Op.(tmp * v.m_diff.(i))
        ) v.m_diff
    end; res

  let ( &** ) v v' =
    let res = lift Op.(v &** v'.m_val) in
    if depend v' then begin
      let tmp = Op.(res.m_val *& (scalar_log v)) in
      setDepend res v';
      Array.iteri (fun i _ ->
          res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i))
        ) v'.m_diff
    end; res

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

  let scalar_log = Op.scalar_log

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
