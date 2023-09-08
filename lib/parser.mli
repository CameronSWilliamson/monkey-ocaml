type t =
  { lexer : Lexer.t
  ; current : Token.token
  ; next : Token.token
  ; debug : bool
  }
[@@deriving show]

val init: Lexer.t -> t
val parse: t -> (Ast.node, string) result
