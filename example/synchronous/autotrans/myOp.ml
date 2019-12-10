include Fadbad.F(Fadbad.OpFloat)

let min_max x y = if x > y then (y,x) else (x,y)
let min x y = fst (min_max x y)
let max x y = snd (min_max x y)

let bad_sgn e =
  let zero = zero () in
  if e = zero then 0.0 else if e > zero then 1.0 else -1.0

let ( =? ) = ( = )
let ( <>? ) = ( <> )
let ( >? ) = ( > )
let ( >=? ) = ( >= )
let ( <? ) = ( < )
let ( <=? ) = ( <= )

let cAdd a b = ignore (a += b)
