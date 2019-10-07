module Op = Fadbad.Op.OpFloat

module type OpFloatS = Fadbad.Op.S with type elt = float and type scalar = float

module type DiffFloatS =
  sig
    include OpFloatS

    val diff: t -> int -> int -> unit
    val value: t -> 'a
    val d: t -> int -> 'a
  end

let get_one_gaussian_by_summation () =
  let rec sum_rand acc n =
    if n = 0 then
      acc
    else
      sum_rand (acc +. (Random.float 1.)) (n - 1)
  in
  (sum_rand 0. 12) -. 6.


let get_one_gaussian_by_Box_Muller () =
  let rec aux x y =
    let size_squared = (x *. x) +. (y *. y) in
    if size_squared < 1. then
      x *. sqrt( -2. *. log(size_squared) /. size_squared)
    else
      let x = 2. *. (Random.float 1.) -. 1. in
      let y = 2. *. (Random.float 1.) -. 1. in
      aux x y
  in
  aux 10. 10.

module SimpleMonteCarlo (Op : OpFloatS) =
  struct
    let run expiry strike spot vol r nb_paths =
      let open Op in
      let variance = scale (vol * vol) expiry in
      let root_variance = sqrt variance in
      let ito_correction = scale variance (-0.5) in
      let moved_spot = spot * (exp ((scale r  expiry) + ito_correction)) in
      let rec aux sum nb =
        if Pervasives.(nb <= 0) then
          sum
        else
          let this_gaussian = get_one_gaussian_by_Box_Muller () in
          let this_spot = moved_spot * (exp (scale root_variance this_gaussian)) in
          let this_payoff = this_spot - strike in
          let this_payoff = if Pervasives.((Op.get this_payoff) > 0.) then this_payoff else Op.zero () in
          aux (sum + this_payoff) Pervasives.(nb - 1)
      in
      let mean = scale (aux (make 0.) nb_paths) (1. /. (float_of_int nb_paths)) in
      mean * (exp (scale r (-. expiry)))
  end

module BADSimpleMonteCarlo (Op : DiffFloatS) =
  struct
    module SimpleMonteCarlo = SimpleMonteCarlo(Op)

    type price =
      {
        price: Op.t;
        d_price_spot: Op.t;
        d_price_vol: Op.t;
        d_price_r: Op.t;
      }

    let create_price () =
      let open Op in
      {
        price = zero ();
        d_price_spot = zero ();
        d_price_vol = zero ();
        d_price_r = zero ();
      }

    let run expiry strike spot vol r nb_paths =
      let open Op in
      let module SMC = SimpleMonteCarlo in
      let b_spot = make spot in
      let b_vol = make vol in
      let b_r = make r in
      let b_price = SMC.run expiry strike b_spot b_vol b_r nb_paths in
      let () = diff b_price 0 1 in
      {
        price = value b_price;
        d_price_spot = d b_spot 0;
        d_price_vol = d b_vol 0;
        d_price_r = d b_r 0;
      }
  end

module FADSimpleMonteCarlo (Op : DiffFloatS) =
  struct
    module SimpleMonteCarlo = SimpleMonteCarlo(Op)

    type price =
      {
        price: Op.t;
        d_price_spot: Op.t;
        d_price_vol: Op.t;
        d_price_r: Op.t;
      }

    let create_price () =
      let open Op in
      {
        price = zero ();
        d_price_spot = zero ();
        d_price_vol = zero ();
        d_price_r = zero ();
      }

    let run expiry strike spot vol r nb_paths =
      let open Op in
      let module SMC = SimpleMonteCarlo in
      let f_spot = make spot in
      let f_vol = make vol in
      let f_r = make r in
      let () = diff f_spot 0 3 in
      let () = diff f_vol 1 3 in
      let () = diff f_r 2 3 in
      let f_price = SMC.run expiry strike f_spot f_vol f_r nb_paths in
      {
        price = value f_price;
        d_price_spot = d f_price 0;
        d_price_vol = d f_price 1;
        d_price_r = d f_price 2;
      }
  end


module FADFADSimpleMonteCarlo (Op : DiffFloatS) =
  struct
    module FSMC = FADSimpleMonteCarlo(Op)

    type price =
      {
        price: Op.t;
        d_price_spot: Op.t;
        d_price_vol: Op.t;
        d_price_r: Op.t;
        d_price_2_spot: Op.t;
      }

    let run expiry strike spot vol r nb_paths =
      let open Op in
      let f_spot = make spot in
      let f_vol = make vol in
      let f_r = make r in
      let () = diff f_spot 0 1 in
      let f = FSMC.run expiry strike f_spot f_vol f_r nb_paths in
      {
        price = value f.price;
        d_price_spot = value f.d_price_spot;
        d_price_vol = value f.d_price_vol;
        d_price_r = value f.d_price_r;
        d_price_2_spot = d f.d_price_spot 0;
      }
  end
