type token =
  | Illegal
  | Eof
  (*Identifiers + literals*)
  | Ident of string
  | Int of string
  (*Operators*)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  (*Comparisons*)
  | Lt
  | Gt
  | Equal
  | NotEqual
  (*Delimiters*)
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  (*Keywords*)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq]

let lookup_ident str =
  match str with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident str
;;

let string_of_token = function
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Equal -> "=="
  | NotEqual -> "!="
  | Comma -> ","
  | Semicolon -> ";"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | other -> String.lowercase_ascii @@ show_token other
;;
