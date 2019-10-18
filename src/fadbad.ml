module type OpS = Op.S
module Op = Op
module OpFloat = Op.OpFloat

module F (Operators : OpS) = Fadiff.FTypeName(Operators)
module B (Operators : OpS) = Badiff.BTypeName(Operators)
module T (Operators : OpS) = Tadiff.TTypeName(Operators)
