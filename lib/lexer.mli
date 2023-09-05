type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

val init: string -> t
val next_token: t -> t * Token.token option
