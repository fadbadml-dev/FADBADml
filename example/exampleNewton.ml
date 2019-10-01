module OpFun =
  struct
    type scalar = float
    type elt = float -> float
    type t = elt ref

    let make f = ref f
    let get f = !f

    let copy f = ref !f

    let zero () = ref (fun _ -> 0.)
    let one () = ref (fun _ -> 1.)
    let two () = ref (fun _ -> 2.)

    let scalar_one = 1.

    let diff_n _ _ _ d =
      Utils.user_assert (d = 0) "diff_n : cannot differentiate a float"
    let d_n v i_l =
      Utils.user_assert (i_l = []) "d_n : cannot get derivative of a float";
      get v

    let ( ~+ ) f = f
    let ( ~- ) f = ref (fun x -> -. !f x)

    let ( + ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) +. (!f2 x)))
    let ( +& ) f a = Stdlib.(ref (fun x -> (!f x) +. a))
    let ( &+ ) a f = Stdlib.(ref (fun x -> a +. (!f x)))

    let ( += ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) +. (!f2 x))); f1
    let ( +&= ) f a = Stdlib.(f := (fun x -> (!f x) +. a)); f

    let ( - ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) -. (!f2 x)))
    let ( -& ) f a = Stdlib.(ref (fun x -> (!f x) -. a))
    let ( &- ) a f = Stdlib.(ref (fun x -> a -. (!f x)))
    let ( &-& ) a b = Stdlib.(a -. b)

    let ( -= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) -. (!f2 x))); f1
    let ( -&= ) f a = Stdlib.(f := (fun x -> (!f x) -. a)); f

    let ( * ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) *. (!f2 x)))
    let ( *& ) f a = Stdlib.(ref (fun x -> (!f x) *. a))
    let ( &* ) a f = Stdlib.(ref (fun x -> a *. (!f x)))

    let ( *= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) *. (!f2 x))); f1
    let ( *&= ) f a = Stdlib.(f := (fun x -> (!f x) *. a)); f

    let ( / ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) /. (!f2 x)))
    let ( /& ) f a = Stdlib.(ref (fun x -> (!f x) /. a))
    let ( &/ ) a f = Stdlib.(ref (fun x -> a /. (!f x)))

    let ( /= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) /. (!f2 x))); f1
    let ( /&= ) f a = Stdlib.(f := (fun x -> (!f x) /. a)); f

    let ( ** ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) ** (!f2 x)))
    let ( **& ) f a = Stdlib.(ref (fun x -> (!f x) ** a))
    let ( &** ) a f = Stdlib.(ref (fun x -> a ** (!f x)))

    let inv f = Stdlib.(ref (fun x -> 1. /. (!f x)))
    let sqr f = Stdlib.(ref (fun x -> let z = !f x in z *. z))

    let sqrt f = Stdlib.(ref (fun x -> sqrt (!f x)))
    let log f = Stdlib.(ref (fun x -> log (!f x)))
    let scalar_log a = Stdlib.(log a)
    let exp f = Stdlib.(ref (fun x -> exp (!f x)))
    let sin f = Stdlib.(ref (fun x -> sin (!f x)))
    let cos f = Stdlib.(ref (fun x -> cos (!f x)))
    let tan f = Stdlib.(ref (fun x -> tan (!f x)))
    let asin f = Stdlib.(ref (fun x -> asin (!f x)))
    let acos f = Stdlib.(ref (fun x -> acos (!f x)))
    let atan f = Stdlib.(ref (fun x -> atan (!f x)))

    let ( = ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( <> ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( < ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( <= ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( > ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( >= ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
  end

module F = Fadbad.F(Fadbad.F(OpFun))

let f_heat1D f =
  let alpha = 1. in
  let dfdxx = F.d_n f [0;0] in
  F.(alpha &* (F.make dfdxx))

let newton_step f x0 dt =
  let open F in
  x0 + (dt &* (f x0))

let newton_integration f x0 dt tEnd =
  let rec aux result x0 t =
    if t > tEnd then
      List.rev result
    else
      let x1 = newton_step f x0 dt in
      aux ((F.get x0) :: result) x1 (t +. dt)
  in
  aux [] x0 0.


let print_fun f min max nb =
  let h = (max -. min) /. (float_of_int nb) in
  let x_l = List.init nb (fun i -> (float_of_int i) *. h +. min) in
  List.iter
    (fun x -> print_endline ((string_of_float x) ^ "\t" ^ (string_of_float (f x))))
    x_l;
  print_newline (); print_newline ()

let () =
  let tEnd = 0.2 in
  let dt = 0.05 in
  let x = F.make (fun x -> x) in
  let () = F.diff_n x 0 1 2 in
  (* let temp0 = F.(inv (1. &+ (sqr (x)))) in *) (* does not work! *)
  let temp0 = F.sin x in
  let result = newton_integration f_heat1D temp0 dt tEnd in
  let min = -3. in
  let max = 3. in
  let nb = 100 in
  let () =
    List.iter
      (fun t -> print_fun t min max nb)
      result
  in
  ()
