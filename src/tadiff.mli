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

(** Automatic Taylor Expansion *)

(** Re-define usual operators to compute values and taylor coefficients
    for elements of type Ty.t.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module TTypeName(Ty : Types.OpS) :
sig
  include Types.OpS with type elt = Ty.elt
                    and type scalar = Ty.scalar

  type op_t = Ty.t
  (** Underlying type *)

  type op = ..

  (** Defined operators. This type can be extended to define custom operators *)
  type op += CONST | SCALE of scalar | TRANS of scalar
  | SIN of Ty.t array | COS of Ty.t array
  | ADD | SUB | MUL | DIV | POW
  | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
  | ASIN | ACOS | ATAN

  val string_of_op : op -> string

  val operator : t -> op
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

(** Extends {!TTypeName} with comparison operators.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedTTypeName(Ty : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = Ty.elt
                           and type scalar = Ty.scalar

  type op_t = Ty.t
  (** Underlying type *)

  type op = ..

  (** Defined operators. This type can be extended to define custom operators *)
  type op += CONST | SCALE of scalar | TRANS of scalar
  | SIN of Ty.t array | COS of Ty.t array
  | ADD | SUB | MUL | DIV | POW
  | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
  | ASIN | ACOS | ATAN

  val string_of_op : op -> string
  val operator : t -> op

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
