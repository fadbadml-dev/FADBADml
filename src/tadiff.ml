open Utils

module type OpS' = Op.S'

module TValues(Op : OpS') =
struct
  type t = {
    mutable n : int;
    values : Op.t array;
  }

  let max_length = 40

  let get_values this = Array.sub this.values 0 this.n

  let to_derivatives values =
    let rec mult_by_fact last_fact i arr =
      if i >= Array.length arr then arr
      else if i = 0 then mult_by_fact 1 1 arr
      else begin
        let new_fact = last_fact * i in
        arr.(i) <- Op.((integer new_fact) * arr.(i));
        mult_by_fact new_fact (i+1) arr
      end
    in mult_by_fact 1 0 (Array.copy values)

  let get_derivatives this = to_derivatives (get_values this)

  let string_of_arr arr =
    Printf.sprintf "[%s]" (String.concat ", " (Array.to_list
      (Array.map Op.to_string arr)))
  let string_of_values this = string_of_arr (get_values this)
  let string_of_derivatives this = string_of_arr (get_derivatives this)
  let to_string = string_of_values


  let copy this = {
    n = this.n;
    values = Array.map Op.copy this.values
  }

  let deepcopy this = {
    n = this.n;
    values = Array.map Op.deepcopy this.values
  }

  let create length = {
    n = 0;
    values = Array.init length (fun _ -> Op.zero ());
  }

  let lift f =
    let res = create max_length in
    res.n <- 1;
    res.values.(0) <- f;
    res

  let lift_sized f length =
    let res = create length in
    res.n <- 1;
    res.values.(0) <- f;
    res

  let make_sized f length = lift_sized (Op.make f) length
  let make f = make_sized f max_length

  let check_bounds this i =
    user_assert (i >= 0 && i < (Array.length this.values))
      ("TValues.get: Index " ^ (string_of_int i)^
        " out of bounds [0," ^ (string_of_int (Array.length this.values)) ^ "]")

  let length this = this.n
  let set_length this n = check_bounds this n; this.n <- n
  let reset this = set_length this 0
  let size this = Array.length this.values

  let fill_from this i e =
    Array.fill this.values i (size this - i) e

  let get this i =
    check_bounds this i;
    this.values.(i)

  let set this i v =
    check_bounds this i;
    this.values.(i) <- v
end

module TTypeName(Op : OpS') =
struct
  module TValues = TValues(Op)

  type elt = Op.elt
  type scalar = Op.scalar

  type op =
    | CONST | SCALE of scalar | TRANS of scalar
    (* while computing the derivatives of SIN or COS, we also compute
       the derivatives of COS or SIN. the argument of the constructor is
       used to store those values (to avoid computing at each call of eval) *)
    | SIN of Op.t array | COS of Op.t array
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
    | ASIN | ACOS | ATAN

  type t = {
    mutable operator : op;
    mutable operands : t array;
    mutable tvalues : TValues.t;
  }

  let tvalues this = TValues.get_values this.tvalues
  let derivatives this = TValues.get_derivatives this.tvalues

  let get_tvalues this = Array.map Op.get (tvalues this)
  let get_derivatives this = Array.map Op.get (derivatives this)

  let string_of_op = function
    | CONST -> "CONST"
    | SCALE f -> Printf.sprintf "SCALE %s" (Op.string_of_scalar f)
    | TRANS f -> Printf.sprintf "TRANS %s" (Op.string_of_scalar f)
    | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
    | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
    | EXP -> "EXP" | LOG -> "LOG" | SIN _ -> "SIN" | COS _ -> "COS" | TAN -> "TAN"
    | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"
  let to_short_string this = string_of_op this.operator
  let rec to_string this =
    (Printf.sprintf "{\n\toperator = %s\n\toperands =\n\t\t[%s]\n\t"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list
        (Array.map to_short_string this.operands))))
    ^
    (Printf.sprintf "tvalues = %s\n\tderivatives = %s\n}"
      (TValues.string_of_values this.tvalues)
      (TValues.string_of_derivatives this.tvalues))
  let string_of_scalar = Op.string_of_scalar
  let string_of_elt = Op.string_of_elt

  let get_operands this i =
    user_assert (i < Array.length this.operands && i >= 0)
      ("BTypeName.get_operands: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (Array.length this.operands)) ^
       "]");
    this.operands.(i)

  let create () = {
    operator = CONST;
    operands = [||];
    tvalues = TValues.create TValues.max_length
  }

  let create_sized length = {
    operator = CONST;
    operands = [||];
    tvalues = TValues.create length
  }

  let lift v = {
    operator = CONST;
    operands = [||];
    tvalues = TValues.lift v
  }

  let make f = {
    operator = CONST;
    operands = [||];
    tvalues = TValues.make f
  }

  let make_sized f length = {
    operator = CONST;
    operands = [||];
    tvalues = TValues.make_sized f length
  }

  let integer i = lift (Op.integer i)
  let zero () = lift (Op.zero ())
  let one () = lift (Op.one ())
  let two () = lift (Op.two ())

  let copy this = {
    operator = this.operator;
    operands = Array.copy this.operands;
    tvalues = TValues.copy this.tvalues
  }

  let rec deepcopy this = {
    operator = this.operator;
    operands = Array.map deepcopy this.operands;
    tvalues = TValues.deepcopy this.tvalues
  }

  let rec reset this =
    match this.operator with
    | CONST ->
      TValues.set_length this.tvalues 1;
      TValues.fill_from this.tvalues 1 (Op.zero ())
    | _ -> TValues.reset this.tvalues; Array.iter reset this.operands

  let value this = TValues.get this.tvalues 0
  let get this = Op.get (value this)

  let deriv this i = TValues.get this.tvalues i
  let d this i = Op.get (deriv this i)

  let length this = TValues.length this.tvalues

  let set this i v =
    TValues.set this.tvalues i v;
    TValues.set_length this.tvalues (i+1)

  let rec eval this k =
    match this.operator with
    | CONST -> k+1
    | SCALE f ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = length this to l-1 do
        TValues.set this.tvalues i Op.(scale (deriv t i) f)
      done;
      TValues.set_length this.tvalues l;
      l
    | TRANS f ->
      let t = get_operands this 0 in
      let l = eval t k in
      if (length this) = 0 then set this 0 Op.(translate (value t) f);
      for i = length this to l-1 do
        TValues.set this.tvalues i (deriv t i)
      done;
      TValues.set_length this.tvalues l;
      l
    | ADD ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      for i = length this to l-1 do
        TValues.set this.tvalues i Op.((deriv t1 i) + (deriv t2 i))
      done;
      TValues.set_length this.tvalues l;
      l
    | SUB ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      for i = length this to l-1 do
        TValues.set this.tvalues i Op.((deriv t1 i) - (deriv t2 i))
      done;
      TValues.set_length this.tvalues l;
      l
    | MUL ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          TValues.set this.tvalues i acc;
          aux (Op.zero ()) (i+1) 0
        end else aux Op.(acc + (deriv t1 j) * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      aux (Op.zero ()) (length this) 0;
      TValues.set_length this.tvalues l;
      l
    | DIV ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          TValues.set this.tvalues i Op.(acc / (value t2));
          aux (deriv t1 (i+1)) (i+1) 1
        end else aux Op.(acc - (deriv t2 j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      aux (deriv t1 (length this)) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | POW -> assert false
    | POS ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = length this to l-1 do
        TValues.set this.tvalues i Op.(+ (deriv t i))
      done;
      TValues.set_length this.tvalues l;
      l
    | NEG ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = length this to l-1 do
        TValues.set this.tvalues i Op.(- (deriv t i))
      done;
      TValues.set_length this.tvalues l;
      l
    | INV ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          TValues.set this.tvalues i Op.(acc / (value t));
          aux (Op.zero ()) (i+1) 1
        end else aux Op.(acc - (deriv t j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(inv (value t));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | SQR ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        let m = (i + 1) / 2 in
        if i >= l then ()
        else if j >= m then begin
          let new_val = Op.((two ()) * acc) in
          let new_val =
            if i mod 2 = 0 then Op.(new_val + (sqr (deriv t m))) else new_val
          in
          TValues.set this.tvalues i new_val;
          aux (Op.zero ()) (i+1) 0
        end else aux Op.(acc + (deriv t j) * (deriv t Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(sqr (value t));
      aux (Op.zero ()) (length this) 0;
      TValues.set_length this.tvalues l;
      l
    | SQRT ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        let m = (i + 1) / 2 in
        if i >= l then ()
        else if j >= m then begin
          let new_val = Op.((two ()) * acc) in
          let new_val =
            if i mod 2 = 0 then Op.(new_val + (sqr (deriv this m))) else new_val
          in
          let new_val =
            Op.(((deriv t i) - new_val) / ((two ()) * (value this)))
          in
          TValues.set this.tvalues i new_val;
          aux (Op.zero ()) (i+1) 1
        end else aux Op.(acc + (deriv this j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(sqrt (value t));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | EXP ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i acc;
          aux (Op.zero ()) (i+1) 0
        end else
          aux
            Op.(acc +
              (((one ()) -
               ((integer j) / (integer i))) *
               (deriv t Stdlib.(i-j)) *
               (deriv this j)))
            i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(exp (value t));
      aux (Op.zero ()) (length this) 0;
      TValues.set_length this.tvalues l;
      l
    | LOG ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i Op.(acc / (value t));
          aux (deriv t (i+1)) (i+1) 1
        end else
          aux
            Op.(acc -
              (((one ()) -
               ((integer j) / (integer i))) *
               (deriv t j) *
               (deriv this Stdlib.(i-j))))
            i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(log (value t));
      let i = length this in
      aux (deriv t i) i 1;
      TValues.set_length this.tvalues l;
      l
    | SIN tcoeff_cos ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc_this acc_cos i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i Op.(acc_this / (integer i));
          tcoeff_cos.(i) <- Op.(acc_cos / (integer i));
          aux (Op.zero ()) (Op.zero ()) (i+1) 0
        end else
          aux
            Op.(acc_this + (integer Stdlib.(j+1)) * tcoeff_cos.(Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            Op.(acc_cos - (integer Stdlib.(j+1)) * (deriv this Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            i (j+1)
      in
      if (length this) = 0 then begin
        set this 0 Op.(sin (value t));
        tcoeff_cos.(0) <- Op.(cos (value t));
      end;
      aux (Op.zero ()) (Op.zero ()) (length this) 0;
      TValues.set_length this.tvalues l;
      l
    | COS tcoeff_sin ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc_this acc_sin i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i Op.(acc_this / (integer i));
          tcoeff_sin.(i) <- Op.(acc_sin / (integer i));
          aux (Op.zero ()) (Op.zero ()) (i+1) 0
        end else
          aux
            Op.(acc_this - (integer Stdlib.(j+1)) * tcoeff_sin.(Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            Op.(acc_sin + (integer Stdlib.(j+1)) * (deriv this Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            i (j+1)
      in
      if (length this) = 0 then begin
        set this 0 Op.(cos (value t));
        tcoeff_sin.(0) <- Op.(sin (value t));
      end;
      aux (Op.zero ()) (Op.zero ()) (length this) 0;
      TValues.set_length this.tvalues l;
      l
    | TAN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i
            Op.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (Op.zero ()) (i+1) 1
        end else
          aux Op.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(tan (value t1));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | ASIN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i
            Op.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (Op.zero ()) (i+1) 1
        end else
          aux Op.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(asin (value t1));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | ACOS ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i
            Op.(- ((deriv t1 i) + acc / (integer i)) / (value t2));
          aux (Op.zero ()) (i+1) 1
        end else
          aux Op.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(acos (value t1));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l
    | ATAN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          TValues.set this.tvalues i
            Op.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (Op.zero ()) (i+1) 1
        end else
          aux Op.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (length this) = 0 then set this 0 Op.(atan (value t1));
      aux (Op.zero ()) (length this) 1;
      TValues.set_length this.tvalues l;
      l

  let un_op operator t = {
    operator;
    operands = [|t|];
    tvalues = TValues.create (TValues.size t.tvalues)
  }

  let bin_op operator t1 t2 =
    let length = min (TValues.size t1.tvalues) (TValues.size t2.tvalues) in
    {
      operator;
      operands = [|t1; t2|];
      tvalues = TValues.create length
    }

  let bin_cOp operator t1 t2 =
    let copy_t1 = copy t1 in
    t1.operator <- operator;
    t1.operands <- [|copy_t1; t2|];
    let length = min (TValues.size t1.tvalues) (TValues.size t2.tvalues) in
    t1.tvalues <- TValues.create length;
    t1

  let scale t f = un_op (SCALE f) t
  let translate t f = un_op (TRANS f) t

  let ( ~+ ) = un_op POS
  let ( ~- ) = un_op NEG

  let ( + ) = bin_op ADD
  let ( += ) = bin_cOp ADD

  let ( - ) = bin_op SUB
  let ( -= ) = bin_cOp SUB

  let ( * ) = bin_op MUL
  let ( *= ) = bin_cOp MUL

  let ( / ) = bin_op DIV
  let ( /= ) = bin_cOp DIV

  let inv = un_op INV
  let sqr = un_op SQR
  let sqrt = un_op SQRT
  let log = un_op LOG
  let exp = un_op EXP
  let sin t =
    un_op (SIN (Array.init (TValues.size t.tvalues) (fun _ -> Op.zero ()))) t
  let cos t =
    un_op (COS (Array.init (TValues.size t.tvalues) (fun _ -> Op.zero ()))) t
  (*
     cf. the implementation in FADBAD++ (tadiff.h class TTypeNameTAN and function tan)
     the TAN operator creates a node `sqr (cos x)` in order to use its derivatives
     to compute the n-th derivative of (tan x) with the formula tan x = 1/cos²(x)
  *)
  let tan t = bin_op TAN t (sqr (cos t))
  (* same than TAN *)
  let asin t = bin_op ASIN t (sqrt ((one ()) - (sqr t)))
  (* same than TAN *)
  let acos t = bin_op ACOS t (sqrt ((one ()) - (sqr t)))
  (* same than TAN *)
  let atan t = bin_op ATAN t ((one ()) + (sqr t))

  let ( ** ) t1 t2 = exp (t2 * (log t1))

  let ( = ) t1 t2 = Op.((value t1) = (value t2))
  let ( <> ) t1 t2 = Op.((value t1) <> (value t2))
  let ( < ) t1 t2 = Op.((value t1) < (value t2))
  let ( <= ) t1 t2 = Op.((value t1) <= (value t2))
  let ( > ) t1 t2 = Op.((value t1) > (value t2))
  let ( >= ) t1 t2 = Op.((value t1) >= (value t2))

end
