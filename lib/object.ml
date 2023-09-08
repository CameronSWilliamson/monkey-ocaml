open Base

type t =
  | Integer of int
  | Boolean of bool
  | Null
  | Return of t

let rec inspect = function
  | Integer i -> Fmt.str "%d" i
  | Boolean b -> Fmt.str "%b" b
  | Null -> "null"
  | Return t -> Fmt.str "%s" @@ inspect t
;;

let monkey_true = Boolean true
let monkey_false = Boolean false
let monkey_null = Null
