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

module Op = Op



(** Type of a module of operators *)
module type OpS = Op.S

(** Type of a module of operators over ordered elements *)
module type OrderedOpS = Op.OrderedS

(** Operators over float elements *)
module OpFloat = Op.OpFloat

(** Operators over float elements, including comparison operators *)
module OrderedFloat = Op.OrderedFloat



(** Type of FAD-like modules *)
module type FS = Fadiff.S

(** Type of FAD-like modules over ordered elements *)
module type OrderedFS = Fadiff.OrderedS

(** Construct a FAD-like module from a module of operators *)
module F(Op : Op.S) = Fadiff.FTypeName(Op)

(** Construct a FAD-like module from a module of operators over ordered
    elements *)
module OrderedF(Op : Op.OrderedS) = Fadiff.OrderedFTypeName(Op)



(** Type of BAD-like modules *)
module type BS = Badiff.S

(** Type of BAD-like modules over ordered elements *)
module type OrderedBS = Badiff.OrderedS

(** Construct a BAD-like module from a module of operators *)
module B(Op : Op.S) = Badiff.BTypeName(Op)

(** Construct a BAD-like module from a module of operators over ordered
    elements *)
module OrderedB(Op : Op.OrderedS) = Badiff.OrderedBTypeName(Op)



(** Type of TAD-like modules *)
module type TS = Tadiff.S

(** Construct a TAD-like module from a module of operators *)
module T (Op : OpS) = Tadiff.TTypeName(Op)
