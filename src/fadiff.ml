open Utils

module FTypeName (Op : Op.S) =
struct

  type t = {
    mutable m_val : Op.t;
    mutable m_size : int;
    mutable m_diff : Op.t array;
  }

  let create () = {
    m_val = Op.myZero ();
    m_size = 0;
    m_diff = Array.make 0 (Op.myZero ());
  }

  let make v = { (create ()) with m_val = v; }

  let copy v = {
    m_val = v.m_val;
    m_size = v.m_size;
    m_diff = Array.copy v.m_diff;
  }

  let size v = v.m_size
  let value v = v.m_val
  let get v i = v.m_diff.(i)

  let deriv v i =
    if i < v.m_size then get v i
    else Op.myZero ()

  let d v i = deriv v i

  let diff v idx n =
    user_assert (idx < n) ("Index " ^ (string_of_int idx) ^ " out of bounds [0," ^ (string_of_int n) ^ "]");

    if v.m_size = 0 then begin
      v.m_size <- n;
      v.m_diff <- Array.make n (Op.myZero ());
    end else
      user_assert (v.m_size = n) "derivative vectors not of same size";

    Array.fill v.m_diff 0 v.m_size (Op.myZero ());
    v.m_diff.(idx) <- Op.myOne ();

    v.m_diff.(idx)

  let depend v = v.m_size <> 0

  let setDepend v v' =
    internal_assert (v'.m_size > 0) "input is not a dependent variable";
    if (v.m_size = 0) then begin
      v.m_size <- v'.m_size;
      v.m_diff <- Array.make v.m_size (Op.myZero ());
    end else
      user_assert (v.m_size = v'.m_size) ("derivative vectors not of the same size " ^ (string_of_int v.m_size) ^ "," ^ (string_of_int v'.m_size))

  let setDepend2 v v1 v2 =
    internal_assert (v1.m_size = v2.m_size) ("derivative vectors not of same size " ^ (string_of_int v1.m_size) ^ "," ^ (string_of_int v2.m_size));
    internal_assert (v1.m_size > 0) "lhs-input is not a dependent variable";
    internal_assert (v2.m_size > 0) "rhs-input is not a dependent variable";
    if (v.m_size = 0) then begin
      v.m_size <- v1.m_size;
      v.m_diff <- Array.make v.m_size (Op.myZero ());
    end else
      user_assert (v.m_size = v1.m_size) ("derivative vectors not of the same size " ^ (string_of_int v.m_size) ^ "," ^ (string_of_int v1.m_size))

  let eq v v' = Op.myEq v.m_val v'.m_val
  let ne v v' = Op.myNe v.m_val v'.m_val
  let le v v' = Op.myLe v.m_val v'.m_val
  let lt v v' = Op.myLt v.m_val v'.m_val
  let ge v v' = Op.myGe v.m_val v'.m_val
  let gt v v' = Op.myGt v.m_val v'.m_val

  let eqV v v' = Op.myEq v.m_val v'
  let neV v v' = Op.myNe v.m_val v'
  let leV v v' = Op.myLe v.m_val v'
  let ltV v v' = Op.myLt v.m_val v'
  let geV v v' = Op.myGe v.m_val v'
  let gtV v v' = Op.myGt v.m_val v'

  let add v v' =
    let res = make (Op.myAdd v.m_val v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myAdd v.m_diff.(i) v'.m_diff.(i))
      res.m_diff;
    res

  let sub v v' =
    let res = make (Op.mySub v.m_val v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.mySub v.m_diff.(i) v'.m_diff.(i))
      res.m_diff;
    res

  let mul v v' =
    let res = make (Op.myMul v.m_val v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.myAdd (Op.myMul v.m_diff.(i) v'.m_val)
                                   (Op.myMul v'.m_diff.(i) v.m_val)
      )
      res.m_diff;
    res

  let div v v' =
    let cval = Op.myDiv v.m_val v'.m_val in
    let res = make (cval) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.myDiv (Op.mySub v.m_diff.(i) (Op.myMul cval v'.m_diff.(i)))
                                   v'.m_val
      )
      res.m_diff;
    res

end
