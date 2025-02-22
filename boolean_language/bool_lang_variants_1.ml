
(* From "Real World OCaml (RWO) > GADTs", with disambiguated names for clarity *)

(* Boolean Expression Language *)

(* 
   The `Ill_typed` exception ensures runtime type safety 
   by signaling when an operation is applied to incorrect types.
*)
exception Ill_typed
(* 
   The `value` type defines a restricted set of valid values. 
   This prevents arbitrary types from being used as values.
*)
type value =
  | Int of int
  | Bool of bool

(* 
   The `expr` type defines the valid operations that can be performed.
   This ensures expressions only consist of well-defined operations.
*)
type expr =
  | Value of value      (* A base case where an expression is just a value *)
  | Eq of expr * expr   (* Equality check between two expressions *)
  | Plus of expr * expr (* Addition operation, expected to be used with integers *)
  | If of expr * expr * expr (* Conditional branching based on evaluation *)

(**********************************************************************************)

(* Simple Variant Version *)
let rec eval expr_arg =
  match expr_arg with
  | Value v -> v
  | If (c, t, e) -> (* updated match case from RWO *)
    (match eval c with
     | Bool b -> 
       let t_val = eval t in
       let e_val = eval e in
       (match (t_val, e_val) with
        | Int _, Int _ -> if b then t_val else e_val
        | Bool _, Bool _ -> if b then t_val else e_val
        | _, _ -> raise Ill_typed)
     | Int _ -> raise Ill_typed)
  | Eq (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) -> (* Compiler cannot 'see into' x and y at compile time *)
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Int (f1 + f2))

let i x = Value (Int x)
and b x = Value (Bool x)
and (+:) x y = Plus (x, y)

(* eval i, b, plus tests *)
let _ = eval (i 3)
let _ = eval (b true)
let _ = eval (i 3 +: i 4)
(* Passes typecheck but emits a runtime error
let result_1 = eval (i 3 +: b false)
*)

(* eval If tests *)
let _ = eval (If (b true, b true, b false))
let _ = eval (If (b false, b true, b false))

(* eval Eq tests *)
let _ = eval (Eq (b true, b true))
let _ = eval (Eq (b false, b false))
let _ = eval (Eq (i 3, i 3))