type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr


let i x = Value (Int x)
and b x = Value (Bool x)
and (+:) x y = Plus (x, y)


let _ = i 3
let _ = i 3 +: i 6

(* correctly does not compile
let _ = b 3
let _ = i 3 + b false
*)

let eval_value (type a) (v : a value) : a =
  match v with
  | Int x -> x
  | Bool x -> x

let rec eval : 'a. 'a expr -> 'a = 
  fun (type a) (x : a expr) ->
   match x with
   | Value v -> eval_value v
   | If (c, t, e) -> if eval c then eval t else eval e
   | Eq (x, y) -> eval x = eval y
   | Plus (x, y) -> eval x + eval y

let _ = eval (Value (Bool true))
let _ = eval (If (b true, b true, b false))
let _ = eval (Eq (i 3, i 3))
let _ = eval (Plus (i 3, i 4))
