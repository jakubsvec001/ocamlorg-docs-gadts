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
(* With Module and Phantom Type *)

let rec eval expr_arg =
  match expr_arg with
  | Value v -> v
  | If (c, t, e) ->
    (match eval c with
     | Bool b -> if b then eval t else eval e
     | Int _ -> raise Ill_typed)
  | Eq (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) -> (* Compiler cannot 'see into' x and y at compile time *)
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Int (f1 + f2))
(* 
   Module interface: Defines a **phantom type system** for expressions.
   This enforces type safety at compile time by making the type `'a t` abstract.
   **Note**: Proper type-checking will not work without this interface. 
*)
module type Typesafe_lang_intf = sig
  type 'a t  (* Abstract type: prevents direct construction of invalid terms *)

  (* Typed constructors: These enforce correct types at expression creation time. *)
  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t  (* Ensures condition must be a boolean *)
  val eq : 'a t -> 'a t -> bool t  (* Ensures equality comparisons are type-safe *)
  val plus : int t -> int t -> int t  (* Ensures addition is only performed on integers *)

  (* 
     `int_eval` ensures that only well-typed integer expressions (`int t`) 
     can be evaluated to produce an `int`. The phantom type `'a t` guarantees 
     at compile time that the input is an integer expression, preventing 
     type mismatches before evaluation.
  *)
  val int_eval : int t -> int

  (* 
     `bool_eval` ensures that only well-typed boolean expressions (`bool t`) 
     can be evaluated to produce a `bool`. The phantom type `'a t` guarantees 
     at compile time that the input is a boolean expression, preventing 
     type mismatches before evaluation.
  *)
  val bool_eval : bool t -> bool
end

(* 
   The implementation ensures that expressions cannot be constructed incorrectly.
   The `'a t` phantom type prevents mismatched operations at compile time.
*)
module Typesafe_lang_impl : Typesafe_lang_intf = struct
  type 'a t = expr  (* Phantom type: prevents direct manipulation of `expr` *)

  (* The smart constructors enforce the correct type at construction time. *)
  let int x = Value (Int x)  (* Ensures that only integers are wrapped in `int t` *)
  let bool x = Value (Bool x)  (* Ensures that only booleans are wrapped in `bool t` *)
  let if_ c t e = If (c, t, e)  (* Ensures `c` must be a `bool t` at compile time *)
  let eq x y = Eq (x, y)  (* Ensures `eq` is only used on same-typed expressions *)
  let plus x y = Plus (x, y)  (* Ensures addition only works on `int t` *)

  (* 
     `int_eval` enforces type safety at runtime by ensuring that the result 
     of an expression expected to be an integer is actually an `Int`. 
  *)
  let int_eval expr_arg =
    match eval expr_arg with
    | Int x -> x
    | Bool _ -> raise Ill_typed  (* Raises an error if a boolean is mistakenly used *)

  (* 
     `bool_eval` similarly ensures that only boolean values are processed. 
  *)
  let bool_eval expr_arg =
    match eval expr_arg with
    | Bool x -> x
    | Int _ -> raise Ill_typed  (* Raises an error if an integer is mistakenly used *)
end

(* correctly does not compile
let expression_result = Typesafe_lang_impl.(plus (int 3) (bool false));;
*)

(* evaluate [values] to [bool] or [int] *)
let _ = Typesafe_lang_impl.(bool_eval (bool true))
let _ = Typesafe_lang_impl.(int_eval (int 3))
let _ = Typesafe_lang_impl.(int_eval (plus (int 3) (int 4)))  (* Should return 7 *)

let _ = Typesafe_lang_impl.(bool_eval (eq (int 3) (int 3)))  (* Should return true *)
let _ = Typesafe_lang_impl.(bool_eval (eq (int 3) (int 4)))  (* Should return false *)
let _ = Typesafe_lang_impl.(bool_eval (eq (bool false) (bool false)))  (* unsafe: returns Ill_typed during runtime *)
let _ = Typesafe_lang_impl.(bool_eval (eq (bool true) (bool false)))  (* unsafe: returns Ill_typed during runtime *)
let _ = Typesafe_lang_impl.(bool_eval (eq (bool true) (bool true)))  (* unsafe: returns Ill_typed during runtime *)

let _ = Typesafe_lang_impl.(bool_eval (if_ (bool true) (bool true) (bool false)))
let _ = Typesafe_lang_impl.(bool_eval (if_ (bool true) (bool true) (bool false)))
let _ = Typesafe_lang_impl.(int_eval (if_ (bool true) (int 10) (int 20)))  (* Should return 10 *)
let _ = Typesafe_lang_impl.(int_eval (if_ (bool false) (int 10) (int 20))) (* Should return 20 *)
