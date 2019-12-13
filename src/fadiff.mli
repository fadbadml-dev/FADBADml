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

(** Re-define usual operators to compute values and derivatives for elements of
    type Ty.t in forward mode.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module FTypeName (Ty : Types.OpS) :
sig
  include Types.OpS with type elt = Ty.elt
                    and type scalar = Ty.scalar

  type op_t = Ty.t

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
end

(** Extends {!FTypeName} with comparison operators.
    @param Ty module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedFTypeName(Ty : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = Ty.elt
                           and type scalar = Ty.scalar

  type op_t = Ty.t

  val value : t -> op_t
  val lift : op_t -> t

  val diff : t -> int -> int -> unit
  val d : t -> int -> elt
  val deriv : t -> int -> op_t
end
