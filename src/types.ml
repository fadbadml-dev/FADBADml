(** Module of comparison operators *)
module type Order =
sig
  type t

  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t
end

(** Module of operators, it defines the usual arithmetic operations. *)
module type OpS =
sig

  type t
  type elt
  (** Type of values: this is the type that the user should use with [make]
      and that will be returned by [get] *)

  type scalar
  (** Type of scalars *)
  (* (t, +, .) should be a commutative S-module where S is the set of scalars
     where + : t -> t -> t and . : t -> scalar -> t. *)

  val create : unit -> t
  (* Create an arbitrary value from nothing *)

  val make : elt -> t
  (** Wrap a user-provided value *)

  val integer : int -> t
  (** Wrap an integer *)

  val get : t -> elt
  (** Unwrap a value *)
  val ( !! ) : t -> elt
  (** Alias for [get] *)

  val to_string : t -> string
  val string_of_scalar : scalar -> string
  val string_of_elt : elt -> string

  val copy : t -> t
  val deepcopy : t -> t

  val zero : unit -> t
  (** Construct a fresh value corresponding to 0 *)
  val one : unit -> t
  (** Construct a fresh value corresponding to 1 *)
  val two : unit -> t
  (** Construct a fresh value corresponding to 2 *)

  val scale : t -> scalar -> t
  (** Multiplication between a value and a scalar *)
  val translate : t -> scalar -> t
  (** Addition between a value and a scalar *)

  (** {0 Arithmetic operators} *)

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

  (** {1 Comparison operators} *)

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

(** Extends {!OpS} with comparison operators *)
module type OrderedOpS = sig
  include OpS
  include Order with type t := t
end

(** Extends {!OpS} with functions to compute and retrieve derivatives
    This describes the interface of FAD-like modules *)
module type FTypeS =
sig
  include OpS

  type op_t

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
end

(** Extends {!FTypeS} with comparison operators *)
module type OrderedFTypeS =
sig
  include FTypeS
  include Order with type t := t
end

(** Extends {!OpS} with functions to compute and retrieve derivatives
    This describes the interface of BAD-like modules*)
module type BTypeS =
sig
  include OpS

  type op_t

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
  val compute : t -> unit
  val compute_list : t list -> unit
end

(** Extends {!BTypeS} with comparison operators *)
module type OrderedBTypeS =
sig
  include BTypeS
  include Order with type t := t
end

(** Extends {!OpS} with functions to compute and retrieve taylor coefficients
    This describes the interface of TAD-like modules *)
module type TTypeS =
sig
  include OpS

  type op = ..
  (** Type of defined operators *)

  val string_of_op : op -> string
  val operator : t -> op

  type op_t
  (** Type of underlying values *)

  val value : t -> op_t
  val lift : op_t -> t

  val order : t -> int

  val get_tvalues : t -> elt array
  val get_derivatives : t -> elt array

  val set : t -> int -> op_t -> unit

  val un_op : op -> t -> t
  val bin_op : op -> t -> t -> t

  val d : t -> int -> elt
  val deriv : t -> int -> op_t
  val eval : t -> int -> int
  val reset : t -> unit
end

(** Extends {!TTypeS} with comparison operators *)
module type OrderedTTypeS =
sig
  include TTypeS
  include Order with type t := t
end
