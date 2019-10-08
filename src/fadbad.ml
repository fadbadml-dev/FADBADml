module type OpS = Op.S
module Op = Op
module OpFloat = Op.Make(Op.OpFloat)

module F (Operators : OpS) = Fadiff.FTypeName (Op.Make(Operators))
module B (Operators : OpS) = Badiff.BTypeName (Op.Make(Operators))
module T (Operators : OpS) = Tadiff.TTypeName (Op.Make(Operators))
