module Op = Op.OpFloat
module F = Fadbad.F(Op)
module FF = Fadbad.F(F)


let f_heat1D x =
  let alpha = 1. in
  let dfdx = FF.deriv x 0 in
  let dfdxx = F.d dfdx 0 in
  (*let () = print_endline ("DEBUG: " ^ (string_of_float (Op.get dfdxx))) in*)
  FF.(alpha &* (FF.make dfdxx))

let newton_step f x0 dt =
  let open FF in
  x0 + (dt &* (f x0))

let newton_integration f x0 dt tEnd =
  let rec aux result x0 t =
    if t > tEnd then
      List.rev result
    else
      let x1 = newton_step f x0 dt in
      aux ((FF.get x0) :: result) x1 (t +. dt)
  in
  aux [] x0 0.

let () =
  let tEnd = 100. in
  let dt = 0.001 in
  let temperature x =
    let open FF in
    (* 1 / (1 + (x-2)^2)  +  1 / (1 + (x+2)^2) *)
    (inv (1. &+ (sqr (x -& 2.)))) + (inv (1. &+ (sqr (x +& 2.))))
  in
  let x = F.make 0. in
  let () = F.diff x 0 1 in
  let x = FF.lift x in
  let () = FF.diff x 0 1 in
  (*let () = F.diff (FF.deriv x 0) 0 1 in*)
  let result = newton_integration f_heat1D (temperature x) dt tEnd in
  let () = List.iter (fun t -> print_endline (string_of_float t)) result in
  ()
