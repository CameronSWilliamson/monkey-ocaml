open Base

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let init input =
  if String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let advance lexer =
  if lexer.position >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })
;;

let peek_char lexer =
  if lexer.position >= String.length lexer.input - 1
  then None
  else Some (String.get lexer.input (lexer.position + 1))
;;

let if_peek lexer ch ~not_matched ~when_matched =
  let peeked = peek_char lexer in
  let lexer, result =
    match peeked with
    | Some p when Char.(p = ch) -> advance lexer, when_matched
    | _ -> lexer, not_matched
  in
  advance lexer, result
;;

let seek lexer condition =
  let rec loop lexer = if condition lexer.ch then loop @@ advance lexer else lexer in
  let lexer = loop lexer in
  lexer, lexer.position
;;

let read_while lexer f =
  let start_pos = lexer.position in
  let lexer, pos_end = seek lexer f in
  lexer, String.sub lexer.input ~pos:start_pos ~len:(pos_end - start_pos)
;;

let skip_whitespace lexer =
  let lexer, _ =
    read_while lexer (fun ch ->
      match ch with
      | Some ch -> Char.(ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r')
      | None -> false)
  in
  lexer
;;

let is_letter ch = Char.(ch = '_' || is_alpha ch)

let read_ident lexer =
  let lexer, ident =
    read_while lexer (fun ch ->
      match ch with
      | Some ch -> is_letter ch
      | None -> false)
  in
  lexer, Token.lookup_ident ident
;;

let read_digit lexer =
  let lexer, number =
    read_while lexer (fun ch ->
      match ch with
      | Some ch -> Char.is_digit ch
      | None -> false)
  in
  lexer, Token.Int number
;;

let next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  let lexer, token =
    match lexer.ch with
    | None -> lexer, Eof
    | Some ch ->
      (match ch with
       | '=' -> if_peek lexer '=' ~not_matched:Assign ~when_matched:Equal
       | '!' -> if_peek lexer '=' ~not_matched:Bang ~when_matched:NotEqual
       | '+' -> advance lexer, Plus
       | '-' -> advance lexer, Minus
       | '/' -> advance lexer, Slash
       | '*' -> advance lexer, Asterisk
       | '<' -> advance lexer, Lt
       | '>' -> advance lexer, Gt
       | ',' -> advance lexer, Comma
       | ';' -> advance lexer, Semicolon
       | '(' -> advance lexer, Lparen
       | ')' -> advance lexer, Rparen
       | '{' -> advance lexer, Lbrace
       | '}' -> advance lexer, Rbrace
       | ch when is_letter ch -> read_ident lexer
       | ch when Char.is_digit ch -> read_digit lexer
       | ch -> Fmt.failwith "unknown char: %c" ch)
  in
  match token with
  | Eof -> lexer, None
  | other -> lexer, Some other
;;

module Test = struct
  let input_to_tokens input =
    let lexer = init input in
    let tokens = Vect.create 10 Token.Illegal in
    let rec loop index lexer =
      match next_token lexer with
      | _, None -> Vect.push tokens Token.Eof
      | lexer, Some token ->
        Vect.push tokens token;
        loop (index + 1) lexer
    in
    let _ = loop 0 lexer in
    Vect.to_list tokens
  ;;

  let print_tokens tokens =
    List.iter tokens ~f:(fun token -> Fmt.pr "%s\n" @@ Token.show_token token)
  ;;

  let%expect_test "next token" =
    let input = "=+(){},;" in
    let tokens = input_to_tokens input in
    print_tokens tokens;
    [%expect
      {| 
      Token.Assign
      Token.Plus
      Token.Lparen
      Token.Rparen
      Token.Lbrace
      Token.Rbrace
      Token.Comma
      Token.Semicolon
      Token.Eof |}]
  ;;

  let%expect_test "first code example" =
    let input =
      "let five = 5;\n\
      \    let ten = 10;\n\n\
      \    let add = fn(x, y) {\n\
      \      x + y;\n\
      \    };\n\
      \    \n\
      \    let result = add(five, ten);\n\
      \    !-/*5;\n\
      \    5 < 10 > 5;\n\
      \    if (5 < 10) {\n\
      \        return true;\n\
      \    } else {\n\
      \        return false;\n\
      \    }\n\
      \          10 == 10;\n\
      \          9 != 10;"
    in
    let tokens = input_to_tokens input in
    print_tokens tokens;
    [%expect
      {|
      Token.Let
      (Token.Ident "five")
      Token.Assign
      (Token.Int "5")
      Token.Semicolon
      Token.Let
      (Token.Ident "ten")
      Token.Assign
      (Token.Int "10")
      Token.Semicolon
      Token.Let
      (Token.Ident "add")
      Token.Assign
      Token.Function
      Token.Lparen
      (Token.Ident "x")
      Token.Comma
      (Token.Ident "y")
      Token.Rparen
      Token.Lbrace
      (Token.Ident "x")
      Token.Plus
      (Token.Ident "y")
      Token.Semicolon
      Token.Rbrace
      Token.Semicolon
      Token.Let
      (Token.Ident "result")
      Token.Assign
      (Token.Ident "add")
      Token.Lparen
      (Token.Ident "five")
      Token.Comma
      (Token.Ident "ten")
      Token.Rparen
      Token.Semicolon 
      Token.Bang
      Token.Minus
      Token.Slash
      Token.Asterisk
      (Token.Int "5")
      Token.Semicolon
      (Token.Int "5")
      Token.Lt
      (Token.Int "10")
      Token.Gt
      (Token.Int "5")
      Token.Semicolon
      Token.If
      Token.Lparen
      (Token.Int "5")
      Token.Lt
      (Token.Int "10")
      Token.Rparen
      Token.Lbrace
      Token.Return
      Token.True
      Token.Semicolon
      Token.Rbrace
      Token.Else
      Token.Lbrace
      Token.Return
      Token.False
      Token.Semicolon
      Token.Rbrace
      (Token.Int "10")
      Token.Equal
      (Token.Int "10")
      Token.Semicolon
      (Token.Int "9")
      Token.NotEqual
      (Token.Int "10")
      Token.Semicolon
      Token.Eof |}]
  ;;
end
