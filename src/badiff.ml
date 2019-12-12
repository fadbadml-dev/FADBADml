(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                             Copyright 2019                             *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(** Backward Automatic Differentiation (BAD) *)

open Fadbad_utils

(** Implement an array of derivatives and some useful operators on arrays. *)
module Derivatives (Op : Types.OpS) =
struct
  type t = Op.t array ref

  let create () = ref [||]
  let make n e = Array.init n (fun _ -> Op.copy e)
  let map f this = Array.map f !this
  let copy this = map Op.copy this
  let deepcopy this = map Op.deepcopy this
  let mapi f this = Array.mapi f !this
  let iter f this = Array.iter f !this
  let iteri f this = Array.iteri f !this
  let length this = Array.length !this
  let to_string this =
    Printf.sprintf "[%s]"
      (String.concat ", "
         (Array.to_list (Array.map Op.to_string !this)))

   let rec fprint_t_list ff t_l =
     let rec aux ff t_l =
       match t_l with
       | [] -> ()
       | x :: q -> Format.fprintf ff ";@,%s%a"
                     Op.(string_of_elt !!x) aux q
     in
     match t_l with
     | [] -> ()
     | x :: q -> Format.printf "@[<2>%s%a@]"
                   Op.(string_of_elt !!x) aux q

  let fprint ff this =
    Format.fprintf ff "@[<2>[%a]@]" fprint_t_list (Array.to_list !this)

  let has_values this = length this > 0
  let check_has_values this =
    user_assert (has_values this)
      "Derivatives.check_has_values: Propagating node with no derivatives"

  let check_bounds this i =
    user_assert (i < length this && i >= 0)
      ("Derivatives.check_bounds: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (length this - 1)) ^ "]")

  let get this i =
    if has_values this then begin
      check_bounds this i;
      !this.(i)
    end else Op.zero ()

  let diff this i n =
    user_assert (i < n && i >= 0)
      ("Derivatives.diff: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (n - 1)) ^ "]");
    let res = if has_values this then this else ref (make n (Op.zero ())) in
    !res.(i) <- Op.one ();
    this := !res

  let cAdd v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cAdd: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) += !v'.(i))) v;
    end else v := copy v'

  let cSub v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cSub: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) -= !v'.(i))) v;
    end else v := map Op.(~-) v'

  (** multiply-accumulate operation *)
  let cMac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cMac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) += a * !v'.(i))) v;
    end else v := map (fun v' -> Op.(a * v')) v'

  (** substractive multiply-accumulate operation *)
  let cSmac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cSmac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore Op.(!v.(i) -= a * !v'.(i))) v;
    end else v := map (fun v' -> Op.(- a * v')) v'

end

(** Re-define usual operators to compute values and derivatives for elements of
    type Op.t in backward mode.
    This implements signature {!Types.BTypeS}. *)
module BTypeName (Op : Types.OpS) =
struct
  module D = Derivatives(Op)

  type elt = Op.elt
  type scalar = Op.scalar

  type op_t = Op.t

  type op = ..
  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
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
    mutable value : op_t;
    derivatives : D.t;
  }

  let string_of_op = function
    | CONST -> "CONST"
    | SCALE f -> Printf.sprintf "SCALE %s" (Op.string_of_scalar f)
    | TRANS f -> Printf.sprintf "TRANS %s" (Op.string_of_scalar f)
    | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
    | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
    | EXP -> "EXP" | LOG -> "LOG" | SIN -> "SIN" | COS -> "COS" | TAN -> "TAN"
    | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"
    | _ -> failwith "Unknown operator"

  let to_short_string this = string_of_op this.operator

  let rec fprint_t_list ff t_l =
    let rec aux ff t_l =
      match t_l with
      | [] -> ()
      | x :: q -> Format.fprintf ff ";@,%a%a" fprint_t x aux q
    in
    match t_l with
    | [] -> ()
    | x :: q -> Format.printf "@[<2>%a%a@]" fprint_t x aux q

  and fprint_t ff this =
    let fprint_value ff value =
      Format.fprintf ff "@[<2>value@ =@ %s@]"
        Op.(string_of_elt !!value)
    in
    let fprint_operator ff op =
      Format.fprintf ff "@[<2>operator@ =@ %s@]" (string_of_op op)
    in
    let fprint_operands ff operands =
      Format.fprintf ff "@[<2>operands@ =@ [%a]@]"
        fprint_t_list (Array.to_list operands)
    in
    let fprint_rc ff rc =
      Format.fprintf ff "@[<2>rc@ =@ %d@]" rc
    in
    let fprint_derivatives ff derivatives =
      Format.fprintf ff "@[<2>derivatives@ =@ %a@]"
        D.fprint derivatives
    in
    Format.fprintf ff "@[<2>{@;%a;@;%a;@;%a;@;%a;@;%a;@;}@]"
      fprint_value this.value
      fprint_operator this.operator
      fprint_operands this.operands
      fprint_rc this.rc
      fprint_derivatives this.derivatives

  let fprint ff this = Format.fprintf ff "%a" fprint_t this

  let rec to_string this =
    (Printf.sprintf "{\n\toperator = %s\n\toperands =\n\t\t[%s]\n\t"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list
        (Array.map to_short_string this.operands))))
    ^
    (Printf.sprintf "rc = %d\n\tvalue = %s\n\tderivatives = %s\n}"
      this.rc (Op.to_string this.value) (D.to_string this.derivatives))

  let string_of_scalar = Op.string_of_scalar
  let string_of_elt = Op.string_of_elt

  let add_der this d = D.cAdd this.derivatives d.derivatives
  let sub_der this d = D.cSub this.derivatives d.derivatives
  let mac_der this a d = D.cMac this.derivatives a d.derivatives
  let smac_der this a d = D.cSmac this.derivatives a d.derivatives

  let get_operands this i =
    user_assert (i < Array.length this.operands && i >= 0)
      ("BTypeName.get_operands: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (Array.length this.operands - 1)) ^
       "]");
    this.operands.(i)

  let create () = let v = Op.create () in {
      operator = CONST;
      operands = [||];
      rc = 0;
      value = v;
      derivatives = D.create ();
    }

  let lift v = {
    operator = CONST;
    operands = [||];
    rc = 0;
    value = v;
    derivatives = D.create ();
  }

  let integer i = lift (Op.integer i)
  let make n = lift (Op.make n)
  let zero () = lift (Op.zero ())
  let one () = lift (Op.one ())
  let two () = lift (Op.two ())

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
      value = Op.deepcopy this.value;
      derivatives = ref (D.deepcopy this.derivatives);
    }

  let value this = this.value
  let derivatives this = this.derivatives

  let get this = Op.get this.value
  let ( !! ) = get

  let deriv this i = D.get this.derivatives i
  let d this i = Op.get (deriv this i)

  let propagate this =
    match this.operator with
    | CONST -> ()
    | SCALE f ->
      let t = get_operands this 0 in
      D.cAdd t.derivatives
        (ref (Array.map (fun x -> Op.scale x f) !(this.derivatives)))
    | ADD ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this;
      add_der t2 this
    | SUB ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this;
      sub_der t2 this
    | MUL ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      mac_der t1 (value t2) this;
      mac_der t2 (value t1) this
    | DIV ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let inv_t2 = Op.inv (value t2) in
      mac_der t1 inv_t2 this;
      smac_der t2 Op.(inv_t2 * (value this)) this
    | POW ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let t1_val = value t1 in
      let t2_val = value t2 in
      let tmp1 = Op.(t2_val * (t1_val ** (t2_val - (one ())))) in
      let tmp2 = Op.((value this) * (log t1_val)) in
      mac_der t1 tmp1 this;
      mac_der t2 tmp2 this
    | TRANS _
    | POS ->
      let t = get_operands this 0 in
      add_der t this
    | NEG ->
      let t = get_operands this 0 in
      sub_der t this
    | INV ->
      let t = get_operands this 0 in
      smac_der t Op.(sqr (value this)) this
    | SQR ->
      let t = get_operands this 0 in
      let tmp = Op.((two ()) * (value t)) in
      mac_der t tmp this
    | SQRT ->
      let t = get_operands this 0 in
      let tmp = Op.(inv ((value this) * (two ()))) in
      mac_der t tmp this
    | EXP ->
      let t = get_operands this 0 in
      mac_der t (value this) this
    | LOG ->
      let t = get_operands this 0 in
      mac_der t Op.(inv (value t)) this
    | SIN ->
      let t = get_operands this 0 in
      let tmp = Op.cos (value t) in
      mac_der t tmp this
    | COS ->
      let t = get_operands this 0 in
      let tmp = Op.sin (value t) in
      smac_der t tmp this
    | TAN ->
      let t = get_operands this 0 in
      let tmp = Op.((sqr (value this)) + (one ())) in
      mac_der t tmp this
    | ASIN ->
      let t = get_operands this 0 in
      let tmp = Op.(inv (sqrt ((one ()) - (sqr (value t))))) in
      mac_der t tmp this
    | ACOS ->
      let t = get_operands this 0 in
      let tmp = Op.(inv (sqrt ((one ()) - (sqr (value t))))) in
      smac_der t tmp this
    | ATAN ->
      let t = get_operands this 0 in
      let tmp = Op.(inv ((sqr (value t)) + (one ()))) in
      mac_der t tmp this
    | _ -> failwith "Unknown operator"

  let rec propagateChildren this =
    Array.iter decRef this.operands;
    this.operands <- [||]

  and decRef this =
    user_assert (this.rc > 0) "BTypeName.decRef: Ressource counter negative";
    this.rc <- this.rc - 1;
    if this.rc = 0 then
      if D.has_values this.derivatives then begin
        propagate this;
        propagateChildren this;
      end

  let incRef this = this.rc <- this.rc + 1

  let rec incRef_subtree this =
    incRef this;
    if this.rc = 1 then
      Array.iter incRef_subtree this.operands

  let compute_list t_l =
    List.iter incRef_subtree t_l;
    List.iter decRef t_l

  let compute this = compute_list [this]

  let diff this idx n =
    D.diff this.derivatives idx n

  let un_op operator operation t =
    {
      operator;
      operands = [|t|];
      rc = 0;
      value = operation (value t);
      derivatives = D.create ();
    }

  let bin_op operator operation t1 t2 =
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
    t1.derivatives := [||];
    t1

  let scale t f = un_op (SCALE f) (fun x -> Op.scale x f) t
  let translate t f = un_op (TRANS f) (fun x -> Op.translate x f) t

  let ( ~+ ) = un_op POS Op.(~+)
  let ( ~- ) = un_op NEG Op.(~-)

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

end

(** Extends {!BTypeName} with comparison operators.
    This implements signature {!Types.OrderedBTypeS}. *)
module OrderedBTypeName (Op : Types.OrderedOpS) =
struct
  module OpBTypeName = BTypeName(Op)
  include OpBTypeName

  let ( < ) a b = Op.(value a < value b)
  let ( <= ) a b = Op.(value a <= value b)
  let ( > ) a b = Op.(value a > value b)
  let ( >= ) a b = Op.(value a >= value b)

  let min a b = if a < b then a else b
  let max a b = if a > b then a else b
end
