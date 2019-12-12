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



(** Type of a module of operators *)
module type OpS = Types.OpS

(** Type of a module of operators over ordered elements *)
module type OrderedOpS = Types.OrderedOpS

(** Operators over float elements *)
module OpFloat = Op.Float

(** Operators over float elements, including comparison operators *)
module OrderedFloat = Op.OrderedFloat



(** Type of FAD-like modules *)
module type FS = Types.FTypeS

(** Type of FAD-like modules over ordered elements *)
module type OrderedFS = Types.OrderedFTypeS

(** Construct a FAD-like module from a module of operators *)
module F(Op : OpS) = Fadiff.FTypeName(Op)

(** Construct a FAD-like module from a module of operators over ordered
    elements *)
module OrderedF(Op : OrderedOpS) = Fadiff.OrderedFTypeName(Op)



(** Type of BAD-like modules *)
module type BS = Types.BTypeS

(** Type of BAD-like modules over ordered elements *)
module type OrderedBS = Types.OrderedBTypeS

(** Construct a BAD-like module from a module of operators *)
module B(Op : OpS) = Badiff.BTypeName(Op)

(** Construct a BAD-like module from a module of operators over ordered
    elements *)
module OrderedB(Op : OrderedOpS) = Badiff.OrderedBTypeName(Op)



(** Type of TAD-like modules *)
module type TS = Types.TTypeS

(** Construct a TAD-like module from a module of operators *)
module T (Op : OpS) = Tadiff.TTypeName(Op)
