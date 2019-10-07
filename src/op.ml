module type S =
sig
(** NAMING CONVENTIONS :
    - the operators on values of type t have the same name as the normal
      operators (ie + for add, - for sub, etc...)
    - the binary operators on values of type t and values of type scalar have
      an & in their name (ie t + scalar is called +& and scalar + t is called &+)
    - the operators on values of type scalar have two & in their name
      (ie scalar - scalar is called &-&)
    - some terms have the prefix scalar_ : they are functions on scalars
      (ie scalar_log) or scalar values (scalar_one)
*)

  type t
  type elt
  (** Type of values *)

  type scalar
  (** Type of scalars *)
  (* (t, +, .) should be a commutative S-module where S is the set of scalars *)

  val create : unit -> t
  (* Create an arbitrary value from nothing *)

  val make : elt -> t
  (** Wrap a value *)

  val get : t -> elt
  (** Unwrap a value *)

  val to_string : t -> string

  val string_of_scalar : scalar -> string

  val copy : t -> t

  val zero : unit -> t
  (** Construct a fresh value corresponding to 0 *)
  val one : unit -> t
  (** Construct a fresh value corresponding to 1 *)
  val two : unit -> t
  (** Construct a fresh value corresponding to 2 *)

  val scale : t -> scalar -> t
  val translate : t -> scalar -> t

  val diff_n : t -> int -> int -> int -> unit
  (** [diff_n x i dim n] assigns [i] as index of variable [x] out of [dim]
      up to depth [n] *)

  val d_n : t -> int list -> elt
  (** [d_n f \[i1;...;in\]] returns the value of df/dx1...dxn *)

  val ( ~+ ) : t -> t
  (** unary plus (with copy) *)
  val ( ~- ) : t -> t
  (** unary minus (with copy) *)

  val ( + ) : t -> t -> t
  val ( += ) : t -> t -> t

  val ( - ) : t -> t -> t
  val ( -= ) : t -> t -> t

  val ( * ) : t -> t -> t
  val ( *= ) : t -> t -> t

  val ( / ) : t -> t -> t
  val ( /= ) : t -> t -> t

  val ( ** ) : t -> t -> t

  val inv : t -> t
  val sqr : t -> t

  val sqrt : t -> t
  val log : t -> t
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

  let create () = ref 7895.

  let make x = ref x
  let get f = !f

  let to_string x = string_of_float !x
  let string_of_scalar = string_of_float

  let copy x = ref !x

  let zero () = ref 0.
  let one () = ref 1.
  let two () = ref 2.

  let scale x a = ref (a *. !x)
  let translate x a = ref (!x +. a)

  let diff_n _ _ _ d =
    Utils.user_assert (d = 0) "diff_n : cannot differentiate a float"
  let d_n v i_l =
    Utils.user_assert (i_l = []) "d_n : cannot get derivative of a float";
    get v

  let ( ~+ ) = copy
  let ( ~- ) x = Stdlib.(ref (~-. !x))

  let ( + ) x y = Stdlib.(ref (!x +. !y))
  let ( += ) x y = Stdlib.(x := (!x +. !y)); x

  let ( - ) x y = Stdlib.(ref (!x -. !y))
  let ( -= ) x y = Stdlib.(x := (!x -. !y)); x

  let ( * ) x y = Stdlib.(ref (!x *. !y))
  let ( *= ) x y = Stdlib.(x := (!x *. !y)); x

  let ( / ) x y = Stdlib.(ref (!x /. !y))
  let ( /= ) x y = Stdlib.(x := (!x /. !y)); x

  let ( ** ) x y = Stdlib.(ref (!x ** !y))

  let inv x = Stdlib.(ref (1. /. !x))
  let sqr x = Stdlib.(ref (!x *. !x))

  let sqrt x = Stdlib.(ref (sqrt !x))
  let log x = Stdlib.(ref (log !x))
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
