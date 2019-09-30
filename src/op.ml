(* NAMING CONVENTIONS :
   - the operators on values of type t have the same name as the normal
     operators (ie + for add, - for sub, etc...)
   - the binary operators on values of type t and values of type scalar have
     an & in their name (ie t + scalar is called +& and scalar + t is called &+)
   - the operators on values of type scalar have two & in their name
     (ie scalar - scalar is called &-&)
   - some terms have the prefix scalar_ : they are functions on scalars
     (ie scalar_log) or scalar values (scalar_one)
*)

module type S =
sig
  type t
  type elt
  type scalar

  val make : elt -> t
  val get : t -> elt

  val copy : t -> t

  val zero : unit -> t
  val one : unit -> t
  val two : unit -> t

  val scalar_one : scalar

  val diff_n : t -> int -> int -> int -> unit
  val d_n : t -> int list -> elt

  val ( ~+ ) : t -> t
  val ( ~- ) : t -> t

  val ( + ) : t -> t -> t
  val ( +& ) : t -> scalar -> t
  val ( &+ ) : scalar -> t -> t
  (* val ( &+& ) : scalar -> scalar -> scalar *)

  val ( += ) : t -> t -> t
  val ( +&= ) : t -> scalar -> t

  val ( - ) : t -> t -> t
  val ( -& ) : t -> scalar -> t
  val ( &- ) : scalar -> t -> t
  val ( &-& ) : scalar -> scalar -> scalar

  val ( -= ) : t -> t -> t
  val ( -&= ) : t -> scalar -> t

  val ( * ) : t -> t -> t
  val ( *& ) : t -> scalar -> t
  val ( &* ) : scalar -> t -> t
  (* val ( &*& ) : scalar -> scalar -> scalar *)

  val ( *= ) : t -> t -> t
  val ( *&= ) : t -> scalar -> t

  val ( / ) : t -> t -> t
  val ( /& ) : t -> scalar -> t
  val ( &/ ) : scalar -> t -> t
  (* val ( &/& ) : scalar -> scalar -> scalar *)

  val ( /= ) : t -> t -> t
  val ( /&= ) : t -> scalar -> t

  val ( ** ) : t -> t -> t
  val ( **& ) : t -> scalar -> t
  val ( &** ) : scalar -> t -> t
  (* val ( &**& ) : scalar -> scalar -> scalar *)

  val inv : t -> t
  val sqr : t -> t

  val sqrt : t -> t
  val log : t -> t
  val scalar_log : scalar -> scalar
  val exp : t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan : t -> t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

module OpFloat =
struct
  type t = float ref
  type elt = float
  type scalar = float

  let make x = ref x
  let get f = !f

  let copy x = ref !x

  let zero () = ref 0.
  let one () = ref 1.
  let two () = ref 2.

  let scalar_one = 1.

  let diff_n _ _ _ d =
    Utils.user_assert (d = 0) "diff_n : cannot differentiate a float"
  let d_n v i_l =
    Utils.user_assert (i_l = []) "d_n : cannot get derivative of a float";
    get v

  let ( ~+ ) = copy
  let ( ~+& ) x = x

  let ( ~- ) x = Stdlib.(ref (~-. !x))
  let ( ~-& ) x = Stdlib.(~-. !x)

  let ( + ) x y = Stdlib.(ref (!x +. !y))
  let ( +& ) x y = Stdlib.(ref (!x +. y))
  let ( &+ ) x y = Stdlib.(ref (x +. !y))
  let ( &+& ) = Stdlib.( +. )

  let ( += ) x y = Stdlib.(x := (!x +. !y)); x
  let ( +&= ) x y = Stdlib.(x := (!x +. y)); x

  let ( - ) x y = Stdlib.(ref (!x -. !y))
  let ( -& ) x y = Stdlib.(ref (!x -. y))
  let ( &- ) x y = Stdlib.(ref (x -. !y))
  let ( &-& ) = Stdlib.( -. )

  let ( -= ) x y = Stdlib.(x := (!x -. !y)); x
  let ( -&= ) x y = Stdlib.(x := (!x -. y)); x

  let ( * ) x y = Stdlib.(ref (!x *. !y))
  let ( *& ) x y = Stdlib.(ref (!x *. y))
  let ( &* ) x y = Stdlib.(ref (x *. !y))
  let ( &*& ) = Stdlib.( *. )

  let ( *= ) x y = Stdlib.(x := (!x *. !y)); x
  let ( *&= ) x y = Stdlib.(x := (!x *. y)); x

  let ( / ) x y = Stdlib.(ref (!x /. !y))
  let ( /& ) x y = Stdlib.(ref (!x /. y))
  let ( &/ ) x y = Stdlib.(ref (x /. !y))
  let ( &/& ) = Stdlib.( /. )

  let ( /= ) x y = Stdlib.(x := (!x /. !y)); x
  let ( /&= ) x y = Stdlib.(x := (!x /. y)); x

  let ( ** ) x y = Stdlib.(ref (!x ** !y))
  let ( **& ) x y = Stdlib.(ref (!x ** y))
  let ( &** ) x y = Stdlib.(ref (x ** !y))
  let ( &**& ) = Stdlib.( ** )

  let inv x = Stdlib.(ref (1. /. !x))
  let sqr x = Stdlib.(ref (!x *. !x))

  let sqrt x = Stdlib.(ref (sqrt !x))
  let log x = Stdlib.(ref (log !x))
  let scalar_log x = Stdlib.(log x)
  let exp x = Stdlib.(ref (exp !x))
  let sin x = Stdlib.(ref (sin !x))
  let cos x = Stdlib.(ref (cos !x))
  let tan x = Stdlib.(ref (tan !x))
  let asin x = Stdlib.(ref (asin !x))
  let acos x = Stdlib.(ref (acos !x))
  let atan x = Stdlib.(ref (atan !x))

  let ( = ) x y = Stdlib.(!x = !y)
  let ( <> ) x y = Stdlib.(!x <> !y)
  let ( < ) x y = Stdlib.(!x < !y)
  let ( <= ) x y = Stdlib.(!x <= !y)
  let ( > ) x y = Stdlib.(!x > !y)
  let ( >= ) x y = Stdlib.(!x >= !y)
end
