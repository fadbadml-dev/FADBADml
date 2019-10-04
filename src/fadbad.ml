module type OpS = Op.S
module Op = Op

module F (Op : OpS) = Fadiff.FTypeName (Op)
module B (Op : OpS) = Badiff.BTypeName (Op)
