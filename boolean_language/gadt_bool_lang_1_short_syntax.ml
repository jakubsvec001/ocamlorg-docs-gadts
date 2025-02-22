type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr [@ocaml.warning "-37"]
  | Plus : int expr * int expr -> int expr 
  | If : bool expr * 'a expr * 'a expr -> 'a expr [@ocaml.warning "-37"]


let i x = Value (Int x)
and b x = Value (Bool x)
and (+:) x y = Plus (x, y)


let _ = i 3
let _ = i 3 +: i 6
let _ = b true
let _ = b false

(* correctly does not compile
let _ = b 3
let _ = i 3 + b false
*)

let eval_value : type a. a value -> a = function
  | Int x -> x
  | Bool x -> x

let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y

let _ = eval (Value (Bool true))