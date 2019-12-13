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

(** Backward Automatic Differentiation (BAD) *)

(** Re-define usual operators to compute values and derivatives for elements of
    type Ty.t in backward mode.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module BTypeName (Ty : Types.OpS) :
sig
  include Types.OpS with type elt = Ty.elt
                    and type scalar = Ty.scalar

  type op_t = Ty.t
  (** Underlying type *)

  type op = ..

  (** Defined operators. This type can be extended to define custom operators *)
  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
  val compute : t -> unit
  val compute_list : t list -> unit
end

(** Extends {!BTypeName} with comparison operators.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedBTypeName (Ty : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = Ty.elt
                           and type scalar = Ty.scalar

  type op_t = Ty.t
  (** Underlying type *)

  type op = ..

  (** Defined operators. This type can be extended to define custom operators *)
  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
  val compute : t -> unit
  val compute_list : t list -> unit
end
