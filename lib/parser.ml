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
[@@deriving ord]

let prec_gte a b = compare_precedence a b >= 0
let ( let* ) res f = Base.Result.bind res ~f

let advance parser =
  let lexer, next = Lexer.next_token parser.lexer in
  { parser with lexer; next; current = parser.next }
;;

let error_msg expected actual _line =
  Fmt.str
    "Expected token to be \"%s\", got \"%s\" instead."
    (Token.show_token expected)
    (Token.show_token actual)
;;

let eat parser token location =
  if Token.equal_token token parser.current
  then Ok (advance parser)
  else Error (error_msg token parser.current location)
;;

let eat_next parser token location =
  if Token.equal_token token parser.next
  then Ok (advance parser)
  else Error (error_msg token parser.next location)
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

let debug parser fname =
  if parser.debug
  then
    Fmt.pr
      "parser: (current %s) (next %s) (fname %s)\n"
      (parser.current |> Token.string_of_token)
      (parser.next |> Token.string_of_token)
      fname
;;

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
  let* parser = eat parser Token.Assign Stdlib.__LINE__ in
  let* parser, value = parse_expression parser `Lowest in
  let* parser = eat_next parser Token.Semicolon Stdlib.__LINE__ in
  Ok (parser, Ast.Let { name = ident; value })

and parse_return parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = if peek_is parser Token.Semicolon then advance parser else parser in
  Ok (parser, Ast.Return { value })

and parse_expression_statement parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = advance_if_next parser Token.Semicolon in
  Ok (parser, Ast.ExpressionStmt expr)

and parse_block parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let parser = advance parser in
  let rec parse_block' parser acc =
    let _ = debug parser Stdlib.__FUNCTION__ in
    match parser.current with
    | Token.Rbrace | Token.Eof -> Ok (parser, List.rev acc)
    | _ ->
      let* parser, stmt = parse_statement parser in
      parse_block' (advance parser) (stmt :: acc)
  in
  let* parser, stmt = parse_block' parser [] in
  let _ = debug parser Stdlib.__FUNCTION__ in
  Ok (parser, Ast.{ block = stmt })

and parse_ident parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Ident identifier -> Ok { identifier }
  | other -> Error (error_msg (Token.Ident "name") other Stdlib.__LINE__)

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
  | Token.Bang | Token.Minus -> expr_parse_prefix parser
  | Token.True | Token.False -> expr_parse_bool parser
  | Token.Lparen -> expr_parse_grouped parser
  | Token.If -> expr_parse_if parser
  | Token.Function -> expr_parse_fn parser
  | x -> Error (Fmt.str "no prefix parse function for %s found" @@ Token.show_token x)

and expr_parse_ident parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* identifier = parse_ident parser in
  Ok (parser, Ast.Identifier identifier)

and expr_parse_int parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.Int integer -> Ok (parser, Ast.Integer (Int.of_string integer))
  | other -> Error (error_msg (Token.Int "#") other Stdlib.__LINE__)

and expr_parse_bool parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  match parser.current with
  | Token.True -> Ok (parser, Ast.Boolean true)
  | Token.False -> Ok (parser, Ast.Boolean false)
  | other -> Error (error_msg Token.True other Stdlib.__LINE__)

and expr_parse_prefix parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let op = Token.string_of_token parser.current in
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { op; right })

and expr_parse_grouped parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let parser = advance parser in
  let* parser, exp = parse_expression parser `Lowest in
  if peek_is parser Token.Rparen
  then Ok (advance parser, exp)
  else Error (error_msg Token.Rparen parser.next Stdlib.__LINE__)

and expr_parse_if parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* parser = eat_next parser Token.Lparen Stdlib.__LINE__ in
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = eat_next parser Token.Rparen Stdlib.__LINE__ in
  let* parser = eat_next parser Token.Lbrace Stdlib.__LINE__ in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    if peek_is parser Token.Else
    then (
      let parser = advance parser in
      let* parser = eat_next parser Token.Lbrace Stdlib.__LINE__ in
      let* parser, stmt = parse_block parser in
      Ok (advance parser, Some stmt))
    else Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and expr_parse_fn parser =
  let* parser = eat_next parser Token.Lparen Stdlib.__LINE__ in
  let* parser, parameters = parse_fn_params parser in
  let* parser = eat_next parser Token.Lbrace Stdlib.__LINE__ in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FnLiteral { parameters; body })

and parse_fn_params parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let ident_to_identifier = function
    | Token.Ident identifier -> Ok Ast.{ identifier }
    | other -> Error (error_msg (Token.Ident "name") other Stdlib.__LINE__)
  in
  let rec parse_fn_params' parser acc =
    let _ = debug parser Stdlib.__FUNCTION__ in
    match parser.next with
    | Token.Comma ->
      let parser = advance (advance parser) in
      let* ident = ident_to_identifier parser.current in
      parse_fn_params' parser @@ (ident :: acc)
    | _ -> Ok (parser, List.rev acc)
  in
  match parser.next with
  | Token.Rparen -> Ok (advance parser, [])
  | _ ->
    let parser = advance parser in
    let* ident = ident_to_identifier parser.current in
    let* parser, idents = parse_fn_params' parser [ ident ] in
    let* parser = eat_next parser Token.Rparen Stdlib.__LINE__ in
    Ok (parser, idents)

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
  | Token.Lparen -> Some expr_parse_call
  | _ -> None
(*| x -> Error (Fmt.str "no infix parse function for %s found" @@ Token.show_token x)*)

and expr_parse_infix parser left =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let op = Token.string_of_token parser.current in
  let precedence = token_to_precedence parser.current in
  let parser = advance parser in
  let* parser, right = parse_expression parser precedence in
  Ok (parser, Ast.Infix { left; op; right })

and expr_parse_call parser func =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let* parser, arguments = parse_call_args parser in
  Ok (parser, Ast.CallExpr { func; arguments })

and parse_call_args parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  let rec parse_call_args' parser acc =
    let _ = debug parser Stdlib.__FUNCTION__ in
    match parser.next with
    | Token.Comma ->
      let parser = advance (advance parser) in
      let* parser, expr = parse_expression parser `Lowest in
      parse_call_args' parser @@ (expr :: acc)
    | _ -> Ok (parser, List.rev acc)
  in
  match parser.next with
  | Token.Rparen -> Ok (advance parser, [])
  | _ ->
    let parser = advance parser in
    let* parser, expr = parse_expression parser `Lowest in
    let* parser, arguments = parse_call_args' parser [ expr ] in
    let* parser = eat_next parser Token.Rparen Stdlib.__LINE__ in
    Ok (parser, arguments)

and next_prec parser =
  let _ = debug parser Stdlib.__FUNCTION__ in
  parser.next |> token_to_precedence

and token_to_precedence = function
  | Token.Equal | Token.NotEqual -> `Equals
  | Token.Lt | Token.Gt -> `LessGreater
  | Token.Plus | Token.Minus -> `Sum
  | Token.Slash | Token.Asterisk -> `Product
  | Token.Lparen -> `Call
  | _ -> `Lowest
;;

module Test = struct
  let init_input debug input =
    let lexer = Lexer.init input in
    parse (if debug then init_debug lexer else init lexer)
  ;;

  let expect_program ?(debug = false) input =
    let program = init_input debug input in
    match program with
    | Ok prog -> Ast.print_node prog
    | Error msg -> failwith msg
  ;;

  let expect_error ?(debug = false) input =
    let program = init_input debug input in
    match program with
    | Ok _ -> failwith "No errors"
    | Error msg -> Fmt.pr "%s" msg
  ;;

  let%expect_test "series of let stmts" =
    expect_program {|
  let x = 5;
  let y = true;
  let foobar = y;
  |};
    [%expect
      {|
    Program: [
      LET: let x = 5
      LET: let y = true
      LET: let foobar = y
    ] |}]
  ;;

  let%expect_test "let stmt error" =
    expect_error {|
    let x = 5;
    let y = 20;
    let foobar 828282;
    |};
    [%expect
      {| Expected token to be "Token.Assign", got "(Token.Int "828282")" instead.|}]
  ;;

  let%expect_test "series of return stmts" =
    expect_program {|
  return 5;
  return 10;
  return add(15)
    |};
    [%expect
      {|
    Program: [
      RETURN: return 5
      RETURN: return 10
      RETURN: return add (15)
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
    expect_program "!5;-15;!true;!false;";
    [%expect
      {|
  Program: [
    EXPR: (!5)
    EXPR: (-15)
    EXPR: (!true)
    EXPR: (!false)
  ]|}]
  ;;

  let%expect_test "infix parsing" =
    expect_program
      "5 + 5; 5 - 5; 5 * 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5; true == true; true != \
       false; false == false;";
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
    EXPR: (true == true)
    EXPR: (true != false)
    EXPR: (false == false)
  ]|}]
  ;;

  let%expect_test "operator precedence parsing" =
    expect_program
      {|
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
    true;
    false;
    3 > 5 == false;
    3 < 5 == true;
    1 + (2 + 3) + 4;
    (5 + 5) * 2;
    2 / (5 + 5);
    -(5 + 5);
    !(true == true);
    |};
    [%expect
      {| 
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
    EXPR: true
    EXPR: false
    EXPR: ((3 > 5) == false)
    EXPR: ((3 < 5) == true)
    EXPR: ((1 + (2 + 3)) + 4)
    EXPR: ((5 + 5) * 2)
    EXPR: (2 / (5 + 5))
    EXPR: (-(5 + 5))
    EXPR: (!(true == true))
  ]
    |}]
  ;;

  let%expect_test "if statement" =
    expect_program "if (x < y) { x };if (x < y) { x } else { y }";
    [%expect
      {|
  Program: [
    EXPR: if ((x < y)) BLOCK: [ EXPR: x;]
    EXPR: if ((x < y)) BLOCK: [ EXPR: x;] BLOCK: [ EXPR: y;]
  ]
    |}]
  ;;

  let%expect_test "function literal" =
    expect_program "fn(x, y) { x + y; };fn() {}; fn(x) {}; fn (x, y, z) {}";
    [%expect
      {|
  Program: [
    EXPR: fn(x, y) BLOCK: [ EXPR: (x + y);]
    EXPR: fn() BLOCK: []
    EXPR: fn(x) BLOCK: []
    EXPR: fn(x, y, z) BLOCK: []
  ]
    |}]
  ;;

  let%expect_test "call expression" =
    expect_program "add(1, 2 * 3, 4 + 5);";
    [%expect {|
  Program: [
    EXPR: add (1, (2 * 3), (4 + 5))
  ]
    |}]
  ;;
end
