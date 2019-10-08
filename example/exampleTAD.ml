module OpFloat = Fadbad.OpFloat
module T = Fadbad.T(OpFloat)

let f x y =
  let open T in
  (T.make 2.) ** x

let _ =
  let x = T.make 3. in
  let y = T.make 6. in
  let res = f x y in

  T.set x 1 (OpFloat.one ());
  ignore (T.eval res 10);
  print_endline (T.to_string res);

  T.reset res;

  T.set y 1 (OpFloat.one ());
  ignore (T.eval res 10);
  print_endline (T.to_string res)
