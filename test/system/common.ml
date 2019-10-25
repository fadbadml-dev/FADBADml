let nsteps = 10
let dt = 0.001

type 'a result = {
  exec_time : float;
  dt : float;
  nsteps : int;
  values : 'a;
}

let print_float name ff f = Format.fprintf ff "@[<h>%s@ =@ %f;@]" name f
let print_int name ff i = Format.fprintf ff "@[<h>%s@ =@ %d;@]" name i

let print_float_array name ff a =
  Format.fprintf ff "@[<h>%s@ =@ [%s];@]" name
    (String.concat "; " (Array.to_list (Array.map string_of_float a)))

let print_res f_values ff res =
  Format.fprintf ff "@[<v 2>{@;%a@;%a@;%a@;@[<h>values@ =@ %a;@]@]@;}"
    (print_float "exec_time") res.exec_time
    (print_float "dt") res.dt
    (print_int "nsteps") res.nsteps
    f_values res.values
