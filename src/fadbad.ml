(**************************************************************************)
(*                                                                        *)
(* FADBADml                                                               *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*   Copyright 2019                                                       *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module Op = Op

module type OpS = Op.S
module type OrderedOpS = Op.OrderedS
module OpFloat = Op.OpFloat
module OrderedFloat = Op.OrderedFloat

module type FS = Fadiff.S
module type OrderedFS = Fadiff.OrderedS
module F(Op : Op.S) = Fadiff.FTypeName(Op)
module OrderedF(Op : Op.OrderedS) = Fadiff.OrderedFTypeName(Op)

module type BS = Badiff.S
module type OrderedBS = Badiff.OrderedS
module B(Op : Op.S) = Badiff.BTypeName(Op)
module OrderedB(Op : Op.OrderedS) = Badiff.OrderedBTypeName(Op)

module type TS = Tadiff.S
module T (Op : OpS) = Tadiff.TTypeName(Op)
