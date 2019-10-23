module Index =
  struct
    type t = int

    let next_id = ref 0

    let get_fresh () =
      let id = !next_id in
      let () = next_id := id + 1 in
      id

    let to_string = string_of_int

    let compare i1 i2 = i1 - i2
  end

(* module AffineForm *)
type scalar = float
type elt =
  {
    center: float;
    noises: (Index.t * float) list;
  }

type t =
  {
    mutable t_center: float;
    mutable t_noises: (Index.t * float) list;
  }

let radius x =
  let rec aux r l =
    match l with
    | [] -> r
    | (_,h)::t -> aux (r +. (Float.abs h)) t
  in
  aux 0. x.t_noises

let create_noise () =
  {
    t_center = 0.;
    t_noises = [(Index.get_fresh (), 1.)];
  }

let create () =
  {
    t_center = 0.;
    t_noises = [];
  }

let make aaf =
  {
    t_center = aaf.center;
    t_noises = aaf.noises;
  }

let make_float f =
  {
    t_center = f;
    t_noises = [];
  }

let make_bounds min max =
  {
    t_center = (min +. max) /. 2.;
    t_noises = [(Index.get_fresh (), (max -. min) /. 2.)];
  }

let integer i =
  {
    t_center = float i;
    t_noises = [];
  }

let get x =
  {
    center = x.t_center;
    noises = x.t_noises;
  }

let get_min_max x =
  let radius = radius x in
  let center = x.t_center in
  (center -. radius, center +. radius)

(* gather all noises smaller (absolute value) than threshold *)
let reduce x threshold =
  let rec aux rc rl l =
    match l with
    | [] -> List.rev ((Index.get_fresh (), rc) :: rl)
    | (i,c)::t ->
       let abs = abs_float c in
       if abs < threshold then
         aux (abs +. rc) rl t
       else
         aux rc ((i,c)::rl) t
  in
  { x with t_noises = aux 0. [] x.t_noises }

let noises_to_points lx ly =
  let rec aux r lx ly =
    match lx, ly with
    | [], [] -> r
    | [], hy::ty -> aux ((0.,hy)::r) [] ty
    | hx::tx, [] -> aux ((hx,0.)::r) tx []
    | hx::tx, hy::ty ->
       raise (Failure "TODO")
  in
  aux [] lx ly

let print2d x y =
  let threshold = 1e-3 in
  let x = reduce x threshold in
  let y = reduce y threshold in
  raise (Failure "TODO")

let to_string x =
  String.concat " + "
    ((string_of_float x.t_center)
     :: (List.map
           (fun (i,x) ->
             (string_of_float x) ^ " * e_{" ^ (Index.to_string i) ^ "}")
           x.t_noises))

let string_of_scalar = string_of_float
let string_of_elt x =
  String.concat " + "
    ((string_of_float x.center)
     :: (List.map
           (fun (i,x) ->
             (string_of_float x) ^ " * e_{" ^ (Index.to_string i) ^ "}")
           x.noises))

let copy x =
  {
    t_center = x.t_center;
    t_noises = x.t_noises;
  }

let deepcopy = copy

let zero () = integer 0
let one () = integer 1
let two () = integer 2

let scale x a =
  {
    t_center = a *. x.t_center;
    t_noises = List.map (fun (i,x) -> (i, a *. x)) x.t_noises;
  }

let translate x a =
  {
    (copy x) with t_center = a +. x.t_center;
  }

let ( ~+ ) = copy
let ( ~- ) x = scale x (-1.)

let ( + ) x1 x2 =
  let rec apply_list r l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev r
    | [], _ -> List.rev_append r l2
    | _, [] -> List.rev_append r l1
    | (i1,h1)::t1, (i2,h2)::t2 ->
       let comp = Index.compare i1 i2 in
       if comp = 0 then
         apply_list ((i1, h1 +. h2) :: r) t1 t2
       else if comp < 0 then
         apply_list ((i1,h1) :: r) t1 l2
       else
         apply_list ((i2,h2) :: r) l1 t2
  in
  {
    t_center = x1.t_center +. x2.t_center;
    t_noises = apply_list [] x1.t_noises x2.t_noises;
  }

let ( - ) x1 x2 =
  let rec apply_list r l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev r
    | [], (i2,h2)::t2 -> apply_list ((i2,-.h2)::r) [] t2
    | _, [] -> List.rev_append r l1
    | (i1,h1)::t1, (i2,h2)::t2 ->
       let comp = Index.compare i1 i2 in
       if comp = 0 then
         apply_list ((i1, h1 -. h2) :: r) t1 t2
       else if comp < 0 then
         apply_list ((i1,h1) :: r) t1 l2
       else
         apply_list ((i2,-.h2) :: r) l1 t2
  in
  {
    t_center = x1.t_center -. x2.t_center;
    t_noises = apply_list [] x1.t_noises x2.t_noises;
  }

let ( * ) x1 x2 =
  let c1 = x1.t_center in
  let c2 = x2.t_center in
  let rec aux r l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev ((Index.get_fresh (), (radius x1) *. (radius x2)) :: r)
    | [], (i2,h2)::t2 -> aux ((i2, c1 *. h2) :: r) [] t2
    | (i1,h1)::t1, [] -> aux ((i1, c2 *. h1) :: r) t1 []
    | (i1,h1)::t1, (i2,h2)::t2 ->
       let comp = Index.compare i1 i2 in
       if comp = 0 then
         aux ((i1, (c1 *. h2) +. (c2 *. h1)) :: r) t1 t2
       else if comp < 0 then
         aux ((i1, c2 *. h1) :: r) t1 l2
       else
         aux ((i2, c1 *. h2) :: r) l1 t2
  in
  {
    t_center = c1 *. c2;
    t_noises = aux [] x1.t_noises x2.t_noises;
  }

let ( / ) (x1 : t) (x2 : t) = raise (Failure "( / ) not implemented")


let cumul_op op x1 x2 =
  let x = op x1 x2 in
  let () = x1.t_center <- x.t_center in
  let () = x1.t_noises <- x.t_noises in
  x1

let ( += ) = cumul_op ( + )
let ( -= ) = cumul_op ( - )
let ( *= ) = cumul_op ( * )
let ( /= ) = cumul_op ( / )

let ( ** ) x1 x2 = raise (Failure "( ** ) not implemented")

let inv x = raise (Failure "inv not implemented")
let sqr x = raise (Failure "inv not implemented")
let sqrt x = raise (Failure "inv not implemented")
let log x = raise (Failure "log not implemented")
let exp x = raise (Failure "exp not implemented")
let sin x = raise (Failure "sin not implemented")
let cos x = raise (Failure "cos not implemented")
let tan x = raise (Failure "tan not implemented")
let asin x = raise (Failure "asin not implemented")
let acos x = raise (Failure "acos not implemented")
let atan x = raise (Failure "atan not implemented")

let ( = ) x1 x2 = raise (Failure "( = ) not implemented")
let ( <> ) x1 x2 = raise (Failure "( <> ) not implemented")
let ( < ) x1 x2 = raise (Failure "( < ) not implemented")
let ( <= ) x1 x2 = raise (Failure "( <= ) not implemented")
let ( > ) x1 x2 = raise (Failure "( > ) not implemented")
let ( >= ) x1 x2 = raise (Failure "( >= ) not implemented")


let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let neg = ( ~- )
