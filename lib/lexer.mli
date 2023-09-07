type t =
  { input : string
  ; position : int
  ; ch : char option
  ; debug : bool
  }
[@@deriving show]

val init: string -> t
val next_token: t -> t * Token.token
