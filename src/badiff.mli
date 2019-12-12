module BTypeName (Op : Types.OpS) : Types.BTypeS
                                    with type op_t = Op.t
                                    and type elt = Op.elt
                                    and type scalar = Op.scalar

module OrderedBTypeName (Op : Types.OrderedOpS) : Types.OrderedBTypeS
                                                  with type op_t = Op.t
                                                  and type elt = Op.elt
                                                  and type scalar = Op.scalar
