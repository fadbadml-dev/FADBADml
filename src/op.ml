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
  val mySub : t -> t -> t
  val myMul : t -> t -> t
  val myDiv : t -> t -> t
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
  type t = float

  let myInteger i = float_of_int i

  let myZero () = 0.
  let myOne () = 1.
  let myTwo () = 2.
  let myPI () = 3.14159265359
  let myPos x = +.x
  let myNeg x = -.x
  let myAdd x y = x +. y
  let mySub x y = x -. y
  let myMul x y = x *. y
  let myDiv x y = x /. y
  let myInv x = 1. /. x
  let mySqr x = x *. x

  type x = float
  let myPow x y = x ** y
  let mySqrt x = sqrt x
  let myLog x = log x
  let myExp x = exp x
  let mySin x = sin x
  let myCos x = cos x
  let myTan x = tan x
  let myAsin x = asin x
  let myAcos x = acos x
  let myAtan x = atan x

  let myEq x y = x = y
  let myNe x y = x <> y
  let myLt x y = x < y
  let myLe x y = x <= y
  let myGt x y = x > y
  let myGe x y = x >= y
end
