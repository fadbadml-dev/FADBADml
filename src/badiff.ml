open Utils

module type OpS = Op.S

module Derivatives (Op : OpS) =
struct
  type t = Op.t array ref

  let create () = ref [||]
  let make n e = Array.make n e
  let map f this = Array.map f !this
  let copy this = map Op.copy this
  let mapi f this = Array.mapi f !this
  let iter f this = Array.iter f !this
  let iteri f this = Array.iteri f !this
  let length this = Array.length !this
  let to_string this =
    Printf.sprintf "[%s]"
      (String.concat ", "
        (Array.to_list (Array.map Op.to_string !this)))

  let has_values this = length this > 0
  let check_has_values this =
    user_assert (has_values this) "Propagating node with no derivatives"

  let check_bounds this i =
    user_assert (i < length this && i >= 0)
      ("check_bounds: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (length this)) ^ "]")

  let get this i =
    if has_values this then begin
      check_bounds this i;
      !this.(i)
    end else Op.zero ()

  let diff this i n =
    user_assert (i < n && i >= 0)
      ("diff: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int n) ^ "]");
    let res = if has_values this then this else ref (make n (Op.zero ())) in
    !res.(i) <- Op.one ();
    this := !res

  let cAdd v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("cAdd: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) += !v'.(i))) v;
    end else v := copy v'

  let cSub v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("cSub: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) -= !v'.(i))) v;
    end else v := map Op.(~-) v'

  (** multiply-accumulate operation *)
  let cMac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("cMac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) += a * !v'.(i))) v;
    end else v := map (fun v' -> Op.(a * v')) v'

  (** substractive multiply-accumulate operation *)
  let cSmac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("cSmac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) -= a * !v'.(i))) v;
    end else v := map (fun v' -> Op.(- a * v')) v'

end

module BTypeName (Op : OpS) =
struct
  module D = Derivatives(Op)

  type op =
  | CONST
  | ADD | SUB | MUL | DIV | POW
  | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
  | ASIN | ACOS | ATAN

  (**
    Type of elements
    - [operator] : the operation described by this node
    - [operands] : the arguments of the operator
    - [rc] : the reference counter; it is always equal to the number of nodes
             that points to this sub-expression plus one
    - [value] : the current value of the sub-expression
    - [derivatives] : the derivative of the root expression with respect to
                      this sub-expression
  *)
  type t = {
    mutable operator : op;
    mutable operands : t array;
    mutable rc : int;
    mutable value : Op.t;
    derivatives : D.t;
  }

  type elt = Op.elt
  type scalar = Op.scalar

  let string_of_op = function
  | CONST -> "CONST"
  | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
  | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
  | EXP -> "EXP" | LOG -> "LOG" | SIN -> "SIN" | COS -> "COS" | TAN -> "TAN"
  | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"

  let to_short_string this = string_of_op this.operator

  let rec to_string this =
    Printf.sprintf "{\n\toperator = %s\n\toperands = \n\t\t[%s]\n\trc = %d\n\tvalue = %s\n\tderivatives = %s\n}"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list (Array.map to_string this.operands)))
      this.rc (Op.to_string this.value) (D.to_string this.derivatives)

  let add_der this d = D.cAdd this.derivatives d.derivatives
  let sub_der this d = D.cSub this.derivatives d.derivatives
  let mac_der this a d = D.cMac this.derivatives a d.derivatives
  let smac_der this a d = D.cSmac this.derivatives a d.derivatives

  let get_operands this i =
    user_assert (i < Array.length this.operands && i >= 0)
      ("get_operands: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (Array.length this.operands)) ^ "]");
    this.operands.(i)

  let copy this =
  {
    operator = this.operator;
    operands = Array.copy this.operands;
    rc = this.rc;
    value = Op.copy this.value;
    derivatives = ref (D.copy this.derivatives);
  }

  let rec deepcopy this =
  {
    operator = this.operator;
    operands = Array.map deepcopy this.operands;
    rc = this.rc;
    value = Op.copy this.value;
    derivatives = ref (D.copy this.derivatives);
  }

  let value this = this.value
  let derivatives this = this.derivatives

  let get this = Op.get this.value

  let deriv this i = D.get this.derivatives i
  let d this i = Op.get (deriv this i)

  let propagate this =
    match this.operator with
    | CONST -> ()
    | ADD ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this; add_der t2 this
    | SUB ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this; sub_der t2 this
    | MUL ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      mac_der t1 (value t2) this; mac_der t2 (value t1) this
    | _ -> assert false

  let rec propagateChildren this =
    Array.iter decRef this.operands
    (* this.operands <- [||] *)

  and decRef this =
    user_assert (this.rc > 0) "decRef: Ressource counter negative";
    this.rc <- this.rc - 1;
    if this.rc = 0 then
      if D.has_values this.derivatives then begin
        propagate this;
        propagateChildren this;
      end

  let incRef this = this.rc <- this.rc + 1

  let diff this idx n =
    incRef this;
    D.diff this.derivatives idx n;
    (* decRef cuts the sub-tree below [this] to save some memory space *)
    decRef this;
    (* cut the array of derivatives to save some memory space *)
    this.derivatives := [||]

  (** [d_n f \[i1;...;in\]] returns the value of df/dx{_i1}...dx{_in} *)
  let d_n this i_l =
    user_assert (i_l <> []) "d_n : got empty list";
    Op.d_n (!(this.derivatives).(List.hd i_l)) (List.tl i_l)

  (** [diff_n x i dim n] assigns [i] as index of variable [x] out of [dim]
        up to depth [n] *)
  let diff_n this idx n d =
    if d > 0 then begin
      diff this idx n;
      Op.diff_n (value this) idx n (d-1)
    end

  let make n = let v = Op.make n in {
    operator = CONST;
    operands = [||];
    rc = 0;
    value = v;
    derivatives = D.create ();
  }

  let un_op operator operation t =
  incRef t;
  {
    operator;
    operands = [|t|];
    rc = 0;
    value = operation (value t);
    derivatives = D.create ();
  }

  let bin_op operator operation t1 t2 =
  incRef t1; incRef t2;
  {
    operator;
    operands = [|t1; t2|];
    rc = 0;
    value = operation (value t1) (value t2);
    derivatives = D.create ();
  }

  let bin_cOp operator operation t1 t2 =
    let copy_t1 = copy t1 in
    t1.operator <- operator;
    t1.operands <- [|copy_t1; t2|];
    t1.rc <- 0;
    t1.value <- operation (value t1)  (value t2);
    t1.derivatives := !(D.create ())

  let ( ~+ ) = un_op POS Op.(~-)
  let ( ~- ) = un_op NEG Op.(~+)

  let ( + ) = bin_op ADD Op.( + )
  let ( += ) = bin_cOp ADD Op.( + )

  let ( - ) = bin_op SUB Op.( - )
  let ( -= ) = bin_cOp SUB Op.( - )

  let ( * ) = bin_op MUL Op.( * )
  let ( *= ) = bin_cOp MUL Op.( * )

  let ( / ) = bin_op DIV Op.( / )
  let ( /= ) = bin_cOp DIV Op.( / )

  let ( ** ) = bin_op POW Op.( ** )

  let inv = un_op INV Op.inv
  let sqr = un_op SQR Op.sqr
  let sqrt = un_op SQRT Op.sqrt
  let log = un_op LOG Op.log
  let exp = un_op EXP Op.exp
  let sin = un_op SIN Op.sin
  let cos = un_op COS Op.cos
  let tan = un_op TAN Op.tan
  let asin = un_op ASIN Op.asin
  let acos = un_op ACOS Op.acos
  let atan = un_op ATAN Op.atan

  let ( = ) t1 t2 = Op.((value t1) = (value t2))
  let ( <> ) t1 t2 = Op.((value t1) <> (value t2))
  let ( < ) t1 t2 = Op.((value t1) < (value t2))
  let ( <= ) t1 t2 = Op.((value t1) <= (value t2))
  let ( > ) t1 t2 = Op.((value t1) > (value t2))
  let ( >= ) t1 t2 = Op.((value t1) >= (value t2))

end
