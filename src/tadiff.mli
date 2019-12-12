module TTypeName(Op : Types.OpS) : Types.TTypeS
                                   with type op_t = Op.t
                                   and type elt = Op.elt
                                   and type scalar = Op.scalar

module OrderedTTypeName(Op : Types.OrderedOpS) : Types.OrderedTTypeS
                                                 with type op_t = Op.t
                                                 and type elt = Op.elt
                                                 and type scalar = Op.scalar
