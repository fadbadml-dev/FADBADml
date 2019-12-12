module FTypeName (Op : Types.OpS) : Types.FTypeS
                                    with type op_t = Op.t
                                    and type elt = Op.elt
                                    and type scalar = Op.scalar

module OrderedFTypeName(Op : Types.OrderedOpS) : Types.OrderedFTypeS
                                                 with type op_t = Op.t
                                                 and type elt = Op.elt
                                                 and type scalar = Op.scalar
