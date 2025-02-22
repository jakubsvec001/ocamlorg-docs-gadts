(* More complete printer with Variants *)
exception FormatMismatch of string

type fmt_variant =
  | Int 
  | Float
  | Str

(* match cases cannot type type-check between check*)
type value =
  | VInt of int
  | VFloat of float
  | VStr of string


let rec printf_variant fmt_variant_str args =
  match args with
  | (Int, VInt i) :: rest ->
      printf_variant (Str.global_replace (Str.regexp "%d") (string_of_int i) fmt_variant_str) rest
  | (Str, VStr s) :: rest -> 
      printf_variant (Str.global_replace (Str.regexp "%s") s fmt_variant_str) rest
  | (Float, VFloat f) :: rest -> 
      printf_variant (Str.global_replace (Str.regexp "%f") (string_of_float f) fmt_variant_str) rest
  | [] -> fmt_variant_str
  | _ -> raise (FormatMismatch "Runtime Error: Format specifiers and arguments do not match")



let ex_1_safe = 
  printf_variant
    "%d and %s and %f"
    [(Int, VInt 42); (Str, VStr "hello"); (Float, VFloat 3.14)]

let () = print_endline ex_1_safe

(* Safe: Returns an exception *)
let ex_2_unsafe =
  try
    printf_variant
      "%d and %s and %f"
      [(Str, VStr "hello"); (Float, VInt 42); (Int, VFloat 3.14)]
  with
  | FormatMismatch msg -> msg

let () = print_endline ex_2_unsafe



type fmt_variant_tuple =
  | Int 
  | Float
  | Str

type value_tuple =
  | VInt of int
  | VFloat of float
  | VStr of string

let rec format_string_variant_tuple fmt_str fmt_spec values =
  match (fmt_spec, values) with
  | ([], []) -> fmt_str
  | (Int :: rest_spec, VInt i :: rest_vals) ->
      format_string_variant_tuple (Str.global_replace (Str.regexp "%d") (string_of_int i) fmt_str)
      rest_spec rest_vals
  | (Str :: rest_spec, VStr s :: rest_vals) ->
      format_string_variant_tuple (Str.global_replace (Str.regexp "%s") s fmt_str)
      rest_spec rest_vals
  | (Float :: rest_spec, VFloat f :: rest_vals) ->
      format_string_variant_tuple (Str.global_replace (Str.regexp "%f") (string_of_float f) fmt_str)
      rest_spec rest_vals
  | _ -> raise (FormatMismatch "Runtime Error: Format specifiers and arguments do not match")

(* Example Usage *)
let formatted_string =
  format_string_variant_tuple "%d and %s and %f" [Int; Str; Float] [VInt 42; VStr "hello"; VFloat 3.14]

let () = print_endline formatted_string



(* GADT Example *)

type _ fmt_gadt =
  | Int : 'r fmt_gadt -> (int -> 'r) fmt_gadt
  | Float : 'r fmt_gadt -> (float -> 'r) fmt_gadt
  | Str : 'r fmt_gadt -> (string -> 'r) fmt_gadt
  | Done : string fmt_gadt

let rec printf_gadt : type r. string -> r fmt_gadt -> r =
  fun fmt printf_spec ->
    match printf_spec with
    | Int rest ->  fun i -> printf_gadt (Str.global_replace (Str.regexp "%d") (string_of_int i) fmt) rest
    | Str rest ->  fun s -> printf_gadt (Str.global_replace (Str.regexp "%s") s fmt) rest
    | Float rest ->  fun f -> printf_gadt (Str.global_replace (Str.regexp "%f") (string_of_float f) fmt) rest
    | Done -> fmt
let ex_4_safe = printf_gadt "%d and %s and %f" (Int (Str (Float Done))) 42 "hello" 3.14

let () = print_endline ex_4_safe

(* Incorrect application and will not compile. Therefore safe.
let ex_5_unsafe = printf_gadt "%d and %s and %f" (Str (Int (Float Done))) 42 "hello" 3.14
*)

(*
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let eval : type a. a expr -> a = function
  | Int n -> n + 1
  (* 🚨 Missing case for `Bool`! Causes a runtime exception *)

type packed = Pack : 'a expr -> packed

let eval_packed (Pack e) =
  match e with
  | Int n -> n
  (* 🚨 This doesn’t cover all cases; will cause an error if `Pack (Bool true)` is used *)

*)