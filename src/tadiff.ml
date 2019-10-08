open Utils

module type OpS' = Op.S'

module TValues(Op : OpS') =
struct
  type t = {
    mutable n : int;
    values : Op.t array;
  }

  let to_string this =
    Printf.sprintf "[%s]" (String.concat ", " (Array.to_list
      (Array.map Op.to_string (Array.sub this.values 0 this.n))))

  let to_derivatives this =
    let rec mult_by_fact last_fact i arr =
      if i >= Array.length arr then arr
      else if i = 0 then mult_by_fact 1 1 arr
      else begin
        let new_fact = last_fact * i in
        arr.(i) <- Op.((integer new_fact) * arr.(i));
        mult_by_fact new_fact (i+1) arr
      end
    in
    { this with
      values = mult_by_fact 1 0 (Array.copy this.values) }

  let max_length = 40

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
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  type t = {
    mutable operator : op;
    mutable operands : t array;
    mutable tvalues : TValues.t;
  }

  let string_of_op = function
    | CONST -> "CONST"
    | SCALE f -> Printf.sprintf "SCALE %s" (Op.string_of_scalar f)
    | TRANS f -> Printf.sprintf "TRANS %s" (Op.string_of_scalar f)
    | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
    | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
    | EXP -> "EXP" | LOG -> "LOG" | SIN -> "SIN" | COS -> "COS" | TAN -> "TAN"
    | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"
  let to_short_string this = string_of_op this.operator
  let rec to_string this =
    (Printf.sprintf "{\n\toperator = %s\n\toperands =\n\t\t[%s]\n\t"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list
        (Array.map to_short_string this.operands))))
    ^
    (Printf.sprintf "tvalues = %s\n\tderivatives = %s\n}"
      (TValues.to_string this.tvalues)
      (TValues.to_string (TValues.to_derivatives this.tvalues)))
  let string_of_scalar = Op.string_of_scalar

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
    | SCALE f -> assert false
    | TRANS f -> assert false
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
    | INV -> assert false
    | SQR -> assert false
    | SQRT -> assert false
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
    | SIN -> assert false
    | COS -> assert false
    | TAN -> assert false
    | ASIN -> assert false
    | ACOS -> assert false
    | ATAN -> assert false

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
    t1.tvalues <- TValues.create length

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
  let sin = un_op SIN
  let cos = un_op COS
  let tan = un_op TAN
  let asin = un_op ASIN
  let acos = un_op ACOS
  let atan = un_op ATAN

  let ( ** ) t1 t2 = exp (t2 * (log t1))

  let ( = ) t1 t2 = Op.((value t1) = (value t2))
  let ( <> ) t1 t2 = Op.((value t1) <> (value t2))
  let ( < ) t1 t2 = Op.((value t1) < (value t2))
  let ( <= ) t1 t2 = Op.((value t1) <= (value t2))
  let ( > ) t1 t2 = Op.((value t1) > (value t2))
  let ( >= ) t1 t2 = Op.((value t1) >= (value t2))

end
