open Utils

module FTypeName (Op : Op.S) =
struct

  type t = {
    m_val : Op.t;
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
    user_assert (idx < n)
      ("Index " ^ (string_of_int idx) ^
       " out of bounds [0," ^ (string_of_int n) ^ "]");

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
      v.m_diff <- Array.make v.m_size (Op.myZero ());
    end else
      user_assert (v.m_size = v1.m_size)
        ("derivative vectors not of the same size "
         ^ (string_of_int v.m_size) ^ "," ^ (string_of_int v1.m_size))

  (* ------------------------------ *)
  (* COMPARISON OPERATORS *)
  (* ------------------------------ *)

  let eqV v v' = Op.myEq v.m_val v'
  let vEq v v' = Op.myEq v v'.m_val
  let eq v v' = Op.myEq v.m_val v'.m_val

  let neV v v' = Op.myNe v.m_val v'
  let vNe v v' = Op.myNe v v'.m_val
  let ne v v' = Op.myNe v.m_val v'.m_val

  let leV v v' = Op.myLe v.m_val v'
  let vLe v v' = Op.myLe v v'.m_val
  let le v v' = Op.myLe v.m_val v'.m_val

  let ltV v v' = Op.myLt v.m_val v'
  let vLt v v' = Op.myLt v v'.m_val
  let lt v v' = Op.myLt v.m_val v'.m_val

  let gtV v v' = Op.myGt v.m_val v'
  let vGt v v' = Op.myGt v v'.m_val
  let gt v v' = Op.myGt v.m_val v'.m_val

  let geV v v' = Op.myGe v.m_val v'
  let vGe v v' = Op.myGe v v'.m_val
  let ge v v' = Op.myGe v.m_val v'.m_val

  (* ------------------------------ *)
  (* ARITHMETIC OPERATORS *)
  (* ------------------------------ *)

  (* ADD *)

  let addV v v' =
    let res = make (Op.myAdd v.m_val v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 v.m_size;
    res

  let vAdd v v' =
    let res = make (Op.myAdd v v'.m_val) in
    setDepend res v';
    Array.blit v'.m_diff 0 res.m_diff 0 v'.m_size;
    res

  let add v v' =
    let res = make (Op.myAdd v.m_val v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.myAdd v.m_diff.(i) v'.m_diff.(i))
      res.m_diff;
    res

  let ( ~+ ) v v' =
    match depend v, depend v' with
    | false, false -> make (Op.myAdd v.m_val v'.m_val)
    | true, false -> addV v v'.m_val
    | false, true -> vAdd v.m_val v'
    | true, true -> add v v'

  let ( ~+= ) v v' =
    ignore (Op.myCadd v.m_val v'.m_val);
    if not (depend v') then v
    else begin
      if depend v then
        Array.iteri (fun i vi -> ignore(Op.myCadd vi v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.blit v'.m_diff 0 v.m_diff 0 v.m_size
      end;
      v
    end

  let ( &+= ) v v' = ignore (Op.myCadd v.m_val v'); v

  (* SUB *)

  let subV v v' =
    let res = make (Op.mySub v.m_val v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 v.m_size;
    res

  let vSub v v' =
    let res = make (Op.mySub v v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myNeg v'.m_diff.(i))
      res.m_diff;
    res

  let sub v v' =
    let res = make (Op.mySub v.m_val v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- Op.mySub v.m_diff.(i) v'.m_diff.(i))
      res.m_diff;
    res

  let ( ~- ) v v' =
    match depend v, depend v' with
    | false, false -> make (Op.mySub v.m_val v'.m_val)
    | true, false -> subV v v'.m_val
    | false, true -> vSub v.m_val v'
    | true, true -> sub v v'

  let ( ~-= ) v v' =
    ignore (Op.myCsub v.m_val v'.m_val);
    if not (depend v') then v
    else begin
      if depend v then
        Array.iteri (fun i vi -> ignore(Op.myCsub vi v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.iteri (fun i _ -> v.m_diff.(i) <- Op.myNeg v'.m_diff.(i)) v.m_diff
      end;
      v
    end

  let ( &-= ) v v' = ignore (Op.myCsub v.m_val v'); v

  (* MUL *)

  let mulV v v' =
    let res = make (Op.myMul v.m_val v') in
    setDepend res v;
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) v')
      res.m_diff;
    res

  let vMul v v' =
    let res = make (Op.myMul v v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v'.m_diff.(i) v)
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

  let ( ~* ) v v' =
    match depend v, depend v' with
    | false, false -> make (Op.myMul v.m_val v'.m_val)
    | true, false -> mulV v v'.m_val
    | false, true -> vMul v.m_val v'
    | true, true -> mul v v'

  let ( ~*= ) v v' =
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ -> v.m_diff.(i) <-
          Op.myAdd (Op.myMul v.m_diff.(i) v'.m_val)
            (Op.myMul v'.m_diff.(i) v.m_val))
        v.m_diff
    | true, _ -> Array.iteri (fun i vi -> ignore (Op.myCmul vi v'.m_val))
                   v.m_diff
    | _ ->
      setDepend v v';
      Array.iteri (fun i _ -> v.m_diff.(i) <- Op.myMul v'.m_diff.(i) v.m_val)
        v.m_diff;
    end;
    ignore (Op.myCmul v.m_val v'.m_val);
    v

  let ( &*= ) v v' =
    ignore (Op.myCmul v.m_val v');
    if depend v then v
    else begin
      Array.iteri (fun i vi -> ignore (Op.myCmul vi v')) v.m_diff; v
    end

  (* DIV *)

  let divV v v' =
    let cval = Op.myDiv v.m_val v' in
    let res = make cval in
    if not (depend v) then res
    else begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myDiv v.m_diff.(i) v')
        res.m_diff;
      res
    end

  let vDiv v v' =
    let cval = Op.myDiv v v'.m_val in
    let res = make cval in
    if not (depend v') then res
    else begin
      let tmp = Op.myNeg (Op.myDiv res.m_val v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul tmp v'.m_diff.(i))
        res.m_diff;
      res
    end

  let div v v' =
    let cval = Op.myDiv v.m_val v'.m_val in
    let res = make cval in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          Op.myDiv (Op.mySub v.m_diff.(i) (Op.myMul cval v'.m_diff.(i)))
            v'.m_val
      )
      res.m_diff;
    res

  let ( ~/ ) v v' =
    match depend v, depend v' with
    | false, false -> make (Op.myDiv v.m_val v'.m_val)
    | true, false -> divV v v'.m_val
    | false, true -> vDiv v.m_val v'
    | true, true -> div v v'

  let ( ~/= ) v v' =
    ignore (Op.myCdiv v.m_val v'.m_val);
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            Op.myDiv (Op.mySub v.m_diff.(i) (Op.myMul v.m_val v'.m_diff.(i)))
              v'.m_val)
        v.m_diff
    | true, _ -> Array.iteri (fun i vi -> ignore (Op.myCdiv vi v'.m_val))
                   v.m_diff
    | _ ->
      setDepend v v';
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            Op.myNeg (Op.myDiv (Op.myMul v.m_val v'.m_diff.(i)) v'.m_val))
        v.m_diff;
    end;
    v

  let ( &/= ) v v' =
    ignore (Op.myCmul v.m_val v');
    if depend v then v
    else begin
      Array.iteri (fun i vi -> ignore (Op.myCdiv vi v')) v.m_diff; v
    end

  (* ------------------------------ *)
  (* UNARY FUNCTIONS *)
  (* ------------------------------ *)


  let pos v =
    let res = make (v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.blit v.m_diff 0 res.m_diff 0 v.m_size;
    end;
    res

  let neg v =
    let res = make (Op.myNeg v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myNeg v.m_diff.(i))
        res.m_diff;
    end;
    res

  let sqr v =
    let res = make (Op.mySqr v.m_val) in
    if depend v then begin
      let tmp = Op.myMul (Op.myTwo ()) (v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul tmp v.m_diff.(i))
        res.m_diff;
    end;
    res

  let inv v =
    let res = make (Op.myInv v.m_val) in
    if depend v then begin
      let tmp = Op.myNeg (Op.myInv (Op.mySqr v.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let exp v =
    let res = make (Op.myExp v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) res.m_val)
        res.m_diff;
    end;
    res

  let log v =
    let res = make (Op.myLog v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myDiv v.m_diff.(i) v.m_val)
        res.m_diff;
    end;
    res

  let sqrt v =
    let res = make (Op.mySqrt v.m_val) in
    if depend v then begin
      let tmp = Op.myMul (Op.myTwo ()) res.m_val in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myDiv v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let sin v =
    let res = make (Op.mySin v.m_val) in
    if depend v then begin
      let tmp = Op.myCos v.m_val in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let cos v =
    let res = make (Op.myCos v.m_val) in
    if depend v then begin
      let tmp = Op.myNeg (Op.mySin v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let tan v =
    let res = make (Op.myTan v.m_val) in
    if depend v then begin
      let tmp = Op.myAdd (Op.myOne ()) (Op.mySqr res.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let asin v =
    let res = make (Op.myAsin v.m_val) in
    if depend v then begin
      let tmp =
        Op.myInv (Op.mySqrt (Op.mySub (Op.myOne ()) (Op.mySqr v.m_val))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let acos v =
    let res = make (Op.myAcos v.m_val) in
    if depend v then begin
      let tmp =
        Op.myNeg
          (Op.myInv (Op.mySqrt (Op.mySub (Op.myOne ()) (Op.mySqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  let atan v =
    let res = make (Op.myAtan v.m_val) in
    if depend v then begin
      let tmp = Op.myInv (Op.myAdd (Op.myOne ()) (Op.mySqr v.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul v.m_diff.(i) tmp)
        res.m_diff;
    end;
    res

  (* ------------------------------ *)
  (* BINARY FUNCTIONS *)
  (* ------------------------------ *)

  (* POW *)

  (* let powV v v' =
    let res = make (Op.myPow v.m_val v') in
    if depend v then begin
      let tmp = Op.myMul v' (Op.myPow v.m_val (Op.mySub v' (Op.myOne ()))) in
      res.setDepend v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- Op.myMul tmp v.m_diff.(i))
        res.m_diff
    end;
     res *)

(* NOTE : On devrait renommer le type x de Op en scalar et ajouter les
  fonctions arithmétiques entre Op.t et Op.scalar :
  Pour pow(a,b) on a besoin de faire b * pow(a, b-1) donc il faut avoir
  l'operation * entre Op.scalar et Op.t et l'operation - entre
  Op.scalar et Op.scalar (et la constante 1 de Op.scalar)
   Dans le cas simple (OpFloat), on aura que Op.t = Op.scalar et les
   operations sur les scalaires seront les mêmes que les autres, mais dans
   le cas general (FTypeName(OpFloat)), les operations sur les scalaires
   doivent être redefinies. C'est déjà le cas dans cette classe; ce sont
   les opérations vAdd et addV (en plus de add et (~+))*)

  let powV v v' = assert false
  let vPow v v' = assert false
  let pow v v' = assert false
  let ( ~** ) v v' = assert false


  (* ------------------------------ *)
  (* Op.S INTERFACE *)
  (* ------------------------------ *)

  let myInteger i = make (Op.myInteger i)
  let myZero () = make (Op.myZero ())
  let myOne () = make (Op.myOne ())
  let myTwo () = make (Op.myTwo ())
  let myPI () = make (Op.myPI ())
  let myPos = pos
  let myNeg = neg

  let myAdd = ( ~+ )
  let myCadd = ( ~+= )
  let mySub = ( ~- )
  let myCsub = ( ~-= )
  let myMul = ( ~* )
  let myCmul = ( ~*= )
  let myDiv = ( ~/ )
  let myCdiv = ( ~/= )

  let myInv = inv
  let mySqr = sqr

  type x = Op.x
  let myPow = powV
  let mySqrt = sqrt
  let myLog = log
  let myExp = exp
  let mySin = sin
  let myCos = cos
  let myTan = tan
  let myAsin = asin
  let myAcos = acos
  let myAtan = atan

  let myEq = eq
  let myNe = ne
  let myLt = lt
  let myLe = le
  let myGt = gt
  let myGe = ge

end
