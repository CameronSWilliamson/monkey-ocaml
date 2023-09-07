open Base

type t =
  { input : string
  ; position : int
  ; ch : char option
  ; debug : bool
  }
[@@deriving show]

let init input =
  if String.is_empty input
  then { input; position = 0; ch = None; debug = false }
  else { input; position = 0; ch = Some (String.get input 0); debug = false }
;;

let _init_debug input = { (init input) with debug = true }

let debug lexer fname =
  if lexer.debug
  then (
    let current_item =
      match lexer.ch with
      | Some ch -> String.of_char ch
      | None -> "None"
    in
    Fmt.pr "lexer: %s %s\n" current_item fname)
;;

let advance lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  if lexer.position >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })
;;

let peek_char lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  if lexer.position >= String.length lexer.input - 1
  then None
  else Some (String.get lexer.input (lexer.position + 1))
;;

let if_peek lexer ch ~not_matched ~when_matched =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  let peeked = peek_char lexer in
  let lexer, result =
    match peeked with
    | Some p when Char.(p = ch) -> advance lexer, when_matched
    | _ -> lexer, not_matched
  in
  advance lexer, result
;;

let seek lexer condition =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  let rec loop lexer =
    let _ = debug lexer Stdlib.__FUNCTION__ in
    match lexer.ch with
    | Some ch when condition ch -> loop @@ advance lexer
    | Some _ -> lexer
    | None -> { lexer with position = lexer.position + 1 }
  in
  let lexer = loop lexer in
  lexer, lexer.position
;;

let read_while lexer f =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  let start_pos = lexer.position in
  let lexer, pos_end = seek lexer f in
  let _ = debug lexer @@ Fmt.str "start_pos: %d, end_pos: %d" start_pos pos_end in
  lexer, String.sub lexer.input ~pos:start_pos ~len:(pos_end - start_pos)
;;

let skip_whitespace lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  match lexer.ch with
  | Some _ ->
    let lexer, _ =
      read_while lexer (fun ch -> Char.(ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'))
    in
    lexer
  | None -> lexer
;;

let is_letter ch = Char.(ch = '_' || is_alpha ch)

let read_ident lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  let lexer, ident =
    read_while lexer (fun ch ->
      let _ = debug lexer (Fmt.str "ident %c**" ch) in
      let result = is_letter ch in
      let _ = debug lexer (Fmt.str "%c %b" ch result) in
      result)
  in
  lexer, Token.lookup_ident ident
;;

let read_digit lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
  let lexer, number =
    read_while lexer (fun ch ->
      let _ = debug lexer (Fmt.str "ident %c**" ch) in
      let result = Char.is_digit ch in
      let _ = debug lexer (Fmt.str "%c %b" ch result) in
      result)
  in
  let _ = debug lexer (Fmt.str "aoehutsnaoeuhtsn") in
  let _ = debug lexer (Fmt.str "end number = %s" number) in
  lexer, Token.Int number
;;

let next_token lexer =
  let _ = debug lexer Stdlib.__FUNCTION__ in
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
  lexer, token
;;

module Test = struct
  let input_to_tokens input =
    let lexer = init input in
    let tokens = Vect.create 10 Token.Illegal in
    let rec loop index lexer =
      match next_token lexer with
      | _, Eof -> Vect.push tokens Token.Eof
      | lexer, token ->
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

  let%expect_test "missing semicolon" =
    let tokens = input_to_tokens "let x = 5" in
    print_tokens tokens;
    [%expect
      {|
      Token.Let
      (Token.Ident "x")
      Token.Assign
      (Token.Int "5")
      Token.Eof
      |}]
  ;;
end
