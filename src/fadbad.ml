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

(** Interface to library FADBADml *)

(** {0 Basic types} *)

(** Type of a module of operators *)
module type OpS = Types.OpS

(** Type of a module of operators over ordered elements *)
module type OrderedOpS = Types.OrderedOpS

(** {0 Basic operators} *)

(** Operators over float elements *)
module OpFloat = Op.Float

(** Operators over float elements, including comparison operators *)
module OrderedFloat = Op.OrderedFloat

(** {0 Forward Automatic Differentiation (FAD)} *)

(** Construct a FAD-like module from a module of operators *)
module F(T : OpS) = Fadiff.FTypeName(T)

(** Construct a FAD-like module from a module of operators over ordered
    elements *)
module OrderedF(T : OrderedOpS) = Fadiff.OrderedFTypeName(T)

(** {0 Backward Automatic Differentiation (BAD)} *)

(** Construct a BAD-like module from a module of operators *)
module B(T : OpS) = Badiff.BTypeName(T)

(** Construct a BAD-like module from a module of operators over ordered
    elements *)
module OrderedB(T : OrderedOpS) = Badiff.OrderedBTypeName(T)

(** {0 Automatic Taylor Expansion (TAD)} *)

(** Construct a TAD-like module from a module of operators *)
module T (T : OpS) = Tadiff.TTypeName(T)
