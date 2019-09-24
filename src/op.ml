module type S =
sig
  type t
  val myInteger : int -> t
  val myZero : unit -> t
  val myOne : unit -> t
  val myTwo : unit -> t
  val myPI : unit -> t
  val myPos : t -> t
  val myNeg : t -> t

  val myAdd : t -> t -> t
  val myCadd : t -> t -> t
  val mySub : t -> t -> t
  val myCsub : t -> t -> t
  val myMul : t -> t -> t
  val myCmul : t -> t -> t
  val myDiv : t -> t -> t
  val myCdiv : t -> t -> t

  val myInv : t -> t
  val mySqr : t -> t

  type x
  val myPow : t -> x -> t
  val mySqrt : t -> t
  val myLog : t -> t
  val myExp : t -> t
  val mySin : t -> t
  val myCos : t -> t
  val myTan : t -> t
  val myAsin : t -> t
  val myAcos : t -> t
  val myAtan : t -> t

  val myEq : t -> t -> bool
  val myNe : t -> t -> bool
  val myLt : t -> t -> bool
  val myLe : t -> t -> bool
  val myGt : t -> t -> bool
  val myGe : t -> t -> bool
end

module OpFloat : S =
struct
  type t = float ref

  let myInteger i = ref (float_of_int i)

  let myZero () = ref 0.
  let myOne () = ref 1.
  let myTwo () = ref 2.
  let myPI () = ref 3.14159265359
  let myPos x = x
  let myNeg x = ref (-. !x)

  let myAdd x y = ref (!x +. !y)
  let myCadd x y = x := (!x +. !y); x

  let mySub x y = ref (!x -. !y)
  let myCsub x y = x := (!x -. !y); x

  let myMul x y = ref (!x *. !y)
  let myCmul x y = x := (!x *. !y); x

  let myDiv x y = ref (!x /. !y)
  let myCdiv x y = x := (!x /. !y); x

  let myInv x = ref (1. /. !x)
  let mySqr x = ref (!x *. !x)

  type x = float
  let myPow x y = ref (!x ** y)
  let mySqrt x = ref (sqrt !x)
  let myLog x = ref (log !x)
  let myExp x = ref (exp !x)
  let mySin x = ref (sin !x)
  let myCos x = ref (cos !x)
  let myTan x = ref (tan !x)
  let myAsin x = ref (asin !x)
  let myAcos x = ref (acos !x)
  let myAtan x = ref (atan !x)

  let myEq x y = !x = !y
  let myNe x y = !x <> !y
  let myLt x y = !x < !y
  let myLe x y = !x <= !y
  let myGt x y = !x > !y
  let myGe x y = !x >= !y
end
