open Utils

module type S = Op.S

module FTypeName (Op : S) =
struct

  type t = {
    m_val : Op.t;
    mutable m_size : int;
    mutable m_diff : Op.t array;
  }

  type scalar = Op.scalar

  let create () = {
    m_val = Op.zero ();
    m_size = 0;
    m_diff = Array.make 0 (Op.zero ());
  }

  let make v = { (create ()) with m_val = v; }

  let zero () = make (Op.zero ())
  let one () = make (Op.one ())
  let two () = make (Op.two ())

  let scalar_one = Op.scalar_one

  let copy v = {
    m_val = Op.copy v.m_val;
    m_size = v.m_size;
    m_diff = Array.map Op.copy v.m_diff;
  }

  let size v = v.m_size
  let value v = v.m_val
  let get v i = v.m_diff.(i)

  let deriv v i =
    if i < v.m_size then get v i
    else Op.zero ()

  let d v i = deriv v i

  let diff v idx n =
    user_assert (idx < n)
      ("Index " ^ (string_of_int idx) ^
       " out of bounds [0," ^ (string_of_int n) ^ "]");

    if v.m_size = 0 then begin
      v.m_size <- n;
      v.m_diff <- Array.make n (Op.zero ());
    end else
      user_assert (v.m_size = n) "derivative vectors not of same size";

    Array.fill v.m_diff 0 v.m_size (Op.zero ());
    v.m_diff.(idx) <- Op.one ()

  let depend v = v.m_size <> 0

  let setDepend v v' =
    internal_assert (v'.m_size > 0) "input is not a dependent variable";
    if (v.m_size = 0) then begin
      v.m_size <- v'.m_size;
      v.m_diff <- Array.make v.m_size (Op.zero ());
    end else
      user_assert (v.m_size = v'.m_size)
        ("derivative vectors not of the same size "
         ^ (string_of_int v.m_size) ^ "," ^ (string_of_int v'.m_size))

  let setDepend2 v v1 v2 =
    internal_assert (v1.m_size = v2.m_size)
      ("derivative vectors not of same size "
       ^ (string_of_int v1.m_size) ^ "," ^ (string_of_int v2.m_size));
    internal_assert (v1.m_size > 0) "lhs-input is not a dependent variable";
    internal_assert (v2.m_size > 0) "rhs-input is not a dependent variable";
    if (v.m_size = 0) then begin
      v.m_size <- v1.m_size;
      v.m_diff <- Array.make v.m_size (Op.zero ());
    end else
      user_assert (v.m_size = v1.m_size)
        ("derivative vectors not of the same size "
         ^ (string_of_int v.m_size) ^ "," ^ (string_of_int v1.m_size))

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
    let res = make Op.(v.m_val +& v') in
    if depend v then begin
      setDepend res v;
      Array.blit v.m_diff 0 res.m_diff 0 v.m_size
    end; res

  let ( &+ ) v v' =
    let res = make Op.(v &+ v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.blit v'.m_diff 0 res.m_diff 0 v'.m_size
    end; res

  let addV (v : t) (v' : Op.t) : t =
    let res = make Op.(v.m_val + v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 v.m_size;
    res

  let vAdd v v' =
    let res = make Op.(v + v'.m_val) in
    setDepend res v';
    Array.blit v'.m_diff 0 res.m_diff 0 v'.m_size;
    res

  let add v v' =
    let res = make Op.(v.m_val + v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(v.m_diff.(i) + v'.m_diff.(i)))
      res.m_diff;
    res

  let ( + ) v v' =
    match depend v, depend v' with
    | false, false -> make Op.(v.m_val + v'.m_val)
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
        Array.blit v'.m_diff 0 v.m_diff 0 v.m_size
      end;
      v
    end

  let ( +&= ) v v' = ignore Op.(v.m_val +&= v'); v

  (* SUB *)

  let ( -& ) v v' =
    let res = make Op.(v.m_val -& v') in
    if depend v then begin
      setDepend res v;
      Array.blit v.m_diff 0 res.m_diff 0 v.m_size
    end; res

  let ( &- ) v v' =
    let res = make Op.(v &- v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.( - v'.m_diff.(i) ))
        res.m_diff
    end; res

  let subV v v' =
    let res = make Op.(v.m_val - v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 v.m_size;
    res

  let vSub v v' =
    let res = make Op.(v - v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.( - v'.m_diff.(i) ))
      res.m_diff;
    res

  let sub v v' =
    let res = make Op.(v.m_val - v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.( v.m_diff.(i) - v'.m_diff.(i) ))
      res.m_diff;
    res

  let ( - ) v v' =
    match depend v, depend v' with
    | false, false -> make Op.(v.m_val - v'.m_val)
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
    let res = make Op.(v.m_val *& v') in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) *& v'))
        res.m_diff
    end; res

  let ( &* ) v v' =
    let res = make Op.(v &* v'.m_val) in
    if depend v' then begin
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v &* v'.m_diff.(i)))
        res.m_diff
    end; res

  let mulV v v' =
    let res = make Op.(v.m_val * v') in
    setDepend res v;
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * v'))
      res.m_diff;
    res

  let vMul v v' =
    let res = make Op.(v * v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v'.m_diff.(i) * v))
      res.m_diff;
    res

  let mul v v' =
    let res = make Op.(v.m_val * v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          Op.((v.m_diff.(i) * v'.m_val) + (v'.m_diff.(i) * v.m_val))
      )
      res.m_diff;
    res

  let ( * ) v v' =
    match depend v, depend v' with
    | false, false -> make Op.(v.m_val * v'.m_val)
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
    let res = make cval in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) /& v'))
        res.m_diff
    end;
    res

  let ( &/ ) v v' =
    let cval = Op.(v &/ v'.m_val) in
    let res = make cval in
    if depend v' then begin
      let tmp = Op.(- res.m_val / v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i)))
        res.m_diff
    end;
    res

  let divV v v' =
    let cval = Op.(v.m_val / v') in
    let res = make cval in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / v'))
        res.m_diff
    end;
    res

  let vDiv v v' =
    let cval = Op.(v / v'.m_val) in
    let res = make cval in
    if depend v' then begin
      let tmp = Op.(- res.m_val / v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i)))
        res.m_diff
    end;
    res

  let div v v' =
    let cval = Op.(v.m_val / v'.m_val) in
    let res = make cval in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          Op.((v.m_diff.(i) - (cval * v'.m_diff.(i))) / v'.m_val)
      ) res.m_diff;
    res

  let ( / ) v v' =
    match depend v, depend v' with
    | false, false -> make Op.(v.m_val / v'.m_val)
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
    let res = make Op.(v.m_val **& v') in
    if depend v then begin
      let tmp = Op.(v' &* (v.m_val **& (v' &-& scalar_one))) in
      setDepend res v;
      Array.iteri (fun i _ ->
          res.m_diff.(i) <- Op.(tmp * v.m_diff.(i))
        ) v.m_diff
    end; res

  let ( &** ) v v' =
    let res = make Op.(v &** v'.m_val) in
    if depend v' then begin
      let tmp = Op.(res.m_val *& (scalar_log v)) in
      setDepend res v';
      Array.iteri (fun i _ ->
          res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i))
        ) v'.m_diff
    end; res

  let powV v v' =
    let res = make Op.(v.m_val ** v') in
    let tmp = Op.(v' * (v.m_val ** (v' - (one ())))) in
    setDepend res v;
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(tmp * v.m_diff.(i))
      ) v.m_diff;
    res

  let vPow v v' =
    let res = make Op.(v ** v'.m_val) in
    let tmp = Op.(res.m_val * (log v)) in
    setDepend res v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.(tmp * v'.m_diff.(i))
      ) v'.m_diff;
    res

  let pow v v' =
    let res = make Op.(v.m_val ** v'.m_val) in
    let tmp1 = Op.(v'.m_val * (v.m_val ** (v'.m_val - (one ())))) in
    let tmp2 = Op.(res.m_val * (log v.m_val)) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.((tmp1 * v.m_diff.(i)) + (tmp2 * v'.m_diff.(i)))
      ) v.m_diff;
    res

  let ( ** ) v v' =
    match depend v, depend v' with
    | false, false -> make Op.(v.m_val ** v'.m_val)
    | true, false -> powV v v'.m_val
    | false, true -> vPow v.m_val v'
    | true, true -> pow v v'

  (* ------------------------------ *)
  (* UNARY FUNCTIONS *)
  (* ------------------------------ *)

  let ( ~+ ) v =
    let res = make Op.(+v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(+v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let ( ~- ) v =
    let res = make Op.(- v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(-v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let sqr v =
    let res = make (Op.sqr v.m_val) in
    if depend v then begin
      let tmp = Op.((two ()) * v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(tmp * v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let inv v =
    let res = make Op.(inv v.m_val) in
    if depend v then begin
      let tmp = Op.(- inv (sqr v.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let exp v =
    let res = make Op.(exp v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * res.m_val))
        res.m_diff;
    end;
    res

  let log v =
    let res = make Op.(log v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / v.m_val))
        res.m_diff;
    end;
    res

  let scalar_log = Op.scalar_log

  let sqrt v =
    let res = make Op.(sqrt v.m_val) in
    if depend v then begin
      let tmp = Op.((two ()) * res.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) / tmp))
        res.m_diff;
    end;
    res

  let sin v =
    let res = make Op.(sin v.m_val) in
    if depend v then begin
      let tmp = Op.cos v.m_val in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let cos v =
    let res = make Op.(cos v.m_val) in
    if depend v then begin
      let tmp = Op.(- sin v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let tan v =
    let res = make Op.(tan v.m_val) in
    if depend v then begin
      let tmp = Op.((one ()) + (sqr res.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let asin v =
    let res = make Op.(asin v.m_val) in
    if depend v then begin
      let tmp = Op.(inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let acos v =
    let res = make Op.(acos v.m_val) in
    if depend v then begin
      let tmp = Op.(- inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let atan v =
    let res = make Op.(atan v.m_val) in
    if depend v then begin
      let tmp = Op.(inv ((one ()) + (sqr v.m_val))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res
end
