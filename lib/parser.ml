open Base

type t =
  { lexer : Lexer.t
  ; current : Token.token
  ; next : Token.token
  ; debug : bool
  }
[@@deriving show]

type precedence =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  ]
[@@deriving show, eq, ord]

let prec_gte a b = compare_precedence a b >= 0
let ( >>= ) res f = Base.Result.bind res ~f
let ( let* ) res f = Base.Result.bind res ~f

let bind res f =
  match res with
  | Ok r -> f r
  | o -> o
;;

let advance parser =
  let lexer, next = Lexer.next_token parser.lexer in
  { parser with lexer; next; current = parser.next }
;;

let rec eat_until_semicolon parser =
  match parser.current with
  | Token.Semicolon -> parser
  | _ -> eat_until_semicolon @@ advance parser
;;

let error_msg expected actual =
  Fmt.str
    "expected token to be %s, got %s instead"
    (Token.show_token expected)
    (Token.show_token actual)
;;

let eat parser token =
  if Token.equal_token token parser.current
  then Ok (advance parser)
  else Error (error_msg token parser.current)
;;

let advance_if_next parser token =
  if Token.equal_token token parser.next then advance parser else parser
;;

let peek_is parser token = Token.equal_token parser.next token

let init lexer =
  let parser = { lexer; current = Eof; next = Eof; debug = false } in
  let parser = advance parser in
  let parser = advance parser in
  parser
;;

let init_debug lexer = { (init lexer) with debug = true }
let debug parser fname = if parser.debug then Fmt.pr "%s\n" fname

let rec parse parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let rec parse' parser statements =
    let _ = debug parser Stdlib.__FUNCTION__ in
    match parser.current with
    | Eof -> Ok (parser, List.rev statements)
    | _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> parse' (advance parser) (stmt :: statements)
       | Error msg -> Error msg)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Eof -> Error "End of file reached"
  | Token.Let -> parse_let parser
  | Token.Return -> parse_return parser
  | _ -> parse_expression_statement parser

and parse_let parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let parser = advance parser in
  let* ident = parse_ident parser in
  let parser = advance parser in
  let* parser = eat parser Token.Assign in
  let* parser, value = parse_expression parser `Lowest in
  let parser = eat_until_semicolon parser in
  Ok (parser, Ast.Let { name = ident; value })

and parse_return parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let parser = advance parser in
  let parser = eat_until_semicolon parser in
  Ok (parser, Ast.Return { value = Ast.SomeExpr })

and parse_expression_statement parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = advance_if_next parser Token.Semicolon in
  Ok (parser, Ast.ExpressionStmt expr)

and parse_ident parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Ident identifier -> Ok { identifier }
  | other -> Error (error_msg (Token.Ident "name") other)

and parse_expression parser precedence =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let rec parse_expression' parser left =
    let _ = debug parser Stdlib.__FUNCTION__ in
    if peek_is parser Token.Semicolon || (prec_gte precedence @@ next_prec parser)
    then Ok (parser, left)
    else (
      match infix_parse_fns parser with
      | Some infix_fn ->
        let parser = advance parser in
        let* parser, left = infix_fn parser left in
        parse_expression' parser left
      | None -> Ok (parser, left))
  in
  let* parser, left = prefix_parse_fns parser in
  let* parser, left = parse_expression' parser left in
  Ok (parser, left)

and prefix_parse_fns parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Ident _ -> expr_parse_ident parser
  | Token.Int _ -> expr_parse_int parser
  | Token.Bang | Token.Minus -> expr_parse_prefix parser parser.current
  | x -> Error (Fmt.str "no prefix parse function for %s found" @@ Token.show_token x)

and expr_parse_ident parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* identifier = parse_ident parser in
  Ok (parser, Ast.Identifier identifier)

and expr_parse_int parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Int integer -> Ok (parser, Ast.Integer (Int.of_string integer))
  | other -> Error (error_msg (Token.Int "#") other)

and expr_parse_prefix parser token =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let op = Token.string_of_token token in
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { op; right })

and infix_parse_fns parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.next with
  | Token.Plus
  | Token.Minus
  | Token.Slash
  | Token.Asterisk
  | Token.Equal
  | Token.NotEqual
  | Token.Lt
  | Token.Gt -> Some expr_parse_infix
  | _ -> None
(*| x -> Error (Fmt.str "no infix parse function for %s found" @@ Token.show_token x)*)

and expr_parse_infix parser left =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let op = Token.string_of_token parser.current in
  let precedence = token_to_precedence parser.current in
  let parser = advance parser in
  let* parser, right = parse_expression parser precedence in
  Ok (parser, Ast.Infix { left; op; right })

and cur_prec parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  parser.current |> token_to_precedence

and next_prec parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  parser.next |> token_to_precedence

and token_to_precedence = function
  | Token.Equal | Token.NotEqual -> `Equals
  | Token.Lt | Token.Gt -> `LessGreater
  | Token.Plus | Token.Minus -> `Sum
  | Token.Slash | Token.Asterisk -> `Product
  | _ -> `Lowest
;;

module Test = struct
  let init_input input = parse @@ init @@ Lexer.init input

  let expect_program input =
    let program = init_input input in
    match program with
    | Ok prog -> Ast.print_node prog
    | Error msg -> failwith msg
  ;;

  let expect_error input =
    let program = init_input input in
    match program with
    | Ok _ -> failwith "No errors"
    | Error msg -> Fmt.pr "%s" msg
  ;;

  let%expect_test "series of let stmts" =
    expect_program {|
  let x = 5;
  let y = 20;
  let foobar = 828282;
  |};
    [%expect
      {|
    Program: [
      LET: let x = 5
      LET: let y = 20
      LET: let foobar = 828282
    ] |}]
  ;;

  let%expect_test "let stmt error" =
    expect_error {|
    let x = 5;
    let y = 20;
    let foobar 828282;
    |};
    [%expect {| expected token to be Token.Assign, got (Token.Int "828282") instead |}]
  ;;

  let%expect_test "series of return stmts" =
    expect_program {|
  return 5;
  return 10;
  return add(15);
    |};
    [%expect
      {|
    Program: [
      RETURN: return 5
      RETURN: return 10
      RETURN: return (CallExpr { identifier = "add" } [ (Integer 15) ]
    ] |}]
  ;;

  let%expect_test "identifier expression" =
    expect_program "foobar;";
    [%expect {|
  Program: [
    EXPR: foobar
  ] |}]
  ;;

  let%expect_test "integer literal expression" =
    expect_program "5;";
    [%expect {|
  Program: [
    EXPR: 5
  ] |}]
  ;;

  let%expect_test "prefix parsing" =
    expect_program "!5;-15;";
    [%expect {|
  Program: [
    EXPR: (!5)
    EXPR: (-15)
  ]|}]
  ;;

  let%expect_test "infix parsing" =
    expect_program "5 + 5; 5 - 5; 5 * 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5;";
    [%expect
      {|
  Program: [
    EXPR: (5 + 5)
    EXPR: (5 - 5)
    EXPR: (5 * 5)
    EXPR: (5 / 5)
    EXPR: (5 > 5)
    EXPR: (5 < 5)
    EXPR: (5 == 5)
    EXPR: (5 != 5)
  ]|}]
  ;;

  let%expect_test "operator precedence parsing" =
    (* 304 g
    *)
    expect_program {|
    -a * b;
    !-a;
    a + b + c;
    a + b - c;
    a * b * c;
    a * b / c;
    a + b / c;
    a + b * c + d / e - f;
    3 + 4; -5 * 5;
    5 > 4 == 3 < 4;
    3 + 4 * 5 == 3 * 1 + 4 * 5;
    3 + 4 * 5 == 3 * 1 + 4 * 5;
    |};
    [%expect {| 
  Program: [
    EXPR: ((-a) * b)
    EXPR: (!(-a))
    EXPR: ((a + b) + c)
    EXPR: ((a + b) - c)
    EXPR: ((a * b) * c)
    EXPR: ((a * b) / c)
    EXPR: (a + (b / c))
    EXPR: (((a + (b * c)) + (d / e)) - f)
    EXPR: (3 + 4)
    EXPR: ((-5) * 5)
    EXPR: ((5 > 4) == (3 < 4))
    EXPR: ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))
    EXPR: ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))
  ]
    |}]
  ;;
end
