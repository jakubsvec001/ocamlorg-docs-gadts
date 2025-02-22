type 'a value =
  | Int of 'a
  | Bool of 'a

type 'a expr =
  | Value of 'a value
  | Eq of 'a expr * 'a expr 
  | Plus of 'a expr * 'a expr
  | If of bool expr * 'a expr * 'a expr

let i x = Value (Int x)
and b x = Value (Bool x)
and (+:) x y = Plus (x, y)

let _ = i 3
let _ = b false
let _ = i 3 +: i 4;;

let _ = If (b true, b true, b false) 

let _ = Eq (i 3, i 3)
let _ = Eq (b true, b true)


(* incorrect, but passes typecheck *)
let _ = b 3
(* does not compile
let _ = If (Eq (i 3, i 3), i 0, i 4)
*)