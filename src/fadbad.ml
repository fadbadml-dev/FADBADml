module Op = Op

module type OpS = Op.S
module type OrderedOpS = Op.OrderedS
module OpFloat = Op.OpFloat
module OrderedFloat = Op.OrderedFloat

module type FS = Fadiff.S
module type OrderedFS = Fadiff.OrderedS
module F (Operators : OpS) = Fadiff.FTypeName(Operators)
module OrderedF (Operators : OrderedOpS) = Fadiff.OrderedFTypeName(Operators)

module type BS = Badiff.S
module type OrderedBS = Badiff.OrderedS
module B (Operators : OpS) = Badiff.B(Operators)
module OrderedB (Operators : OrderedOpS) = Badiff.OrderedB(Operators)

module T (Operators : OpS) = Tadiff.TTypeName(Operators)
module type TS = Tadiff.S
