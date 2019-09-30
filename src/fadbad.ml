module Op = Op

module type S = Op.S
module F (Op : S) = Fadiff.FTypeName (Op)
