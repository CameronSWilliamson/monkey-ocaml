let ( let* ) res f = Base.Result.bind res ~f

let rec eval node =
  match node with
  | Ast.Program program -> eval_program program
  | Ast.Expression expr -> eval_expr expr
  | Ast.Statement stmt -> eval_statement stmt

and eval_program stmts =
  let rec eval_program' stmts result =
    match stmts with
    | [] -> Ok result
    | stmt :: rest ->
      let* eval_res = eval_statement stmt in
      (match eval_res with
       | Object.Return value -> Ok value
       | _ -> eval_program' rest eval_res)
  in
  eval_program' stmts.statements Object.monkey_null

and eval_statement = function
  | Ast.ExpressionStmt expr -> eval_expr expr
  | Ast.BlockStmt block -> eval_block_statement block.block
  | Ast.Return expr ->
    let* res = eval_expr expr.value in
    Ok (Object.Return res)
  | _ -> Error "not implemented"

and eval_block_statement block =
  let rec eval_block_statement' result = function
    | [] -> Ok result
    | stmt :: rest ->
      let* res = eval_statement stmt in
      (match res with
       | Object.Return res -> Ok (Object.Return res)
       | _ -> eval_block_statement' res rest)
  in
  eval_block_statement' Object.monkey_null block

and eval_expr = function
  | Ast.Integer integer -> Ok (Object.Integer integer)
  | Ast.Boolean boolean ->
    Ok (if boolean then Object.monkey_true else Object.monkey_false)
  | Ast.Prefix expr ->
    let* right = eval (Ast.Expression expr.right) in
    eval_prefix_expression expr.op right
  | Ast.Infix expr ->
    let* left = eval (Ast.Expression expr.left) in
    let* right = eval (Ast.Expression expr.right) in
    eval_infix_expression expr.op left right
  | Ast.If expr ->
    let* condition = eval_expr expr.condition in
    if is_truthy condition
    then eval_statement (Ast.BlockStmt expr.consequence)
    else (
      match expr.alternative with
      | Some alternative -> eval_statement (Ast.BlockStmt alternative)
      | None -> Ok Object.monkey_null)
  | other -> Error (Fmt.str "%s" @@ Ast.show_expression other)

and eval_prefix_expression op right =
  match op with
  | "!" -> eval_bang_op_expr right
  | "-" -> eval_minus_prefix_op_expr right
  | _ -> Error (Fmt.str "unknown operator: %s%s" op @@ Object.inspect right)

and eval_bang_op_expr = function
  | Object.Boolean true -> Ok Object.monkey_false
  | Object.Boolean false -> Ok Object.monkey_true
  | Object.Null -> Ok Object.monkey_true
  | _ -> Ok Object.monkey_false

and eval_minus_prefix_op_expr = function
  | Object.Integer integer -> Ok (Object.Integer (-1 * integer))
  | other -> Error (Fmt.str "unknown operator: -%s" @@ Object.inspect other)

and eval_infix_expression op right left =
  match right, left with
  | Object.Integer l, Object.Integer r -> eval_int_infix_expr op l r
  | Object.Boolean l, Object.Boolean r -> eval_bool_infix_expr op l r
  | _ -> Ok Object.monkey_null

and eval_int_infix_expr op l r =
  let result =
    match op with
    | "+" -> Object.Integer (l + r)
    | "-" -> Object.Integer (l - r)
    | "/" -> Object.Integer (l / r)
    | "*" -> Object.Integer (l * r)
    | "<" -> Object.Boolean (l < r)
    | ">" -> Object.Boolean (l > r)
    | "==" -> Object.Boolean (l = r)
    | "!=" -> Object.Boolean (l <> r)
    | _ -> Object.monkey_null
  in
  Ok result

and eval_bool_infix_expr op l r =
  let det_result = function
    | true -> Object.monkey_true
    | false -> Object.monkey_false
  in
  let result =
    match op with
    | "==" -> det_result (l = r)
    | "!=" -> det_result (l <> r)
    | _ -> Object.monkey_null
  in
  Ok result

and is_truthy = function
  | Object.Null -> false
  | Object.Boolean true -> true
  | Object.Boolean false -> false
  | _ -> true
;;

module Test = struct
  let evaluate str =
    let ast =
      match Lexer.init str |> Parser.init |> Parser.parse with
      | Ok node -> node
      | Error err -> failwith err
    in
    eval ast
  ;;

  let expect_eval str =
    match evaluate str with
    | Ok obj -> Object.inspect obj |> Fmt.pr "%s\n"
    | Error err -> failwith err
  ;;

  let expect_err str =
    match evaluate str with
    | Ok obj -> failwith @@ Object.inspect obj
    | Error err -> Fmt.pr "%s\n" err
  ;;

  let%expect_test "eval integer" =
    expect_eval "5;";
    expect_eval "10;";
    expect_eval "-10;";
    expect_eval "5 + 5 + 5 - 10";
    expect_eval "2 * 2 * 2 * 2 * 2";
    expect_eval "-50 + 100 + -50";
    expect_eval "5 * 2 + 10";
    expect_eval "5 + 2 * 10";
    expect_eval "20 + 2 * -10";
    expect_eval "50 / 2 * 2 + 10";
    expect_eval "2 * (5 + 10)";
    expect_eval "3 * 3 * 3 + 10";
    expect_eval "3 * (3 * 3) + 10";
    expect_eval "(5 + 10 * 2 + 15 / 3) * 2 + -10";
    [%expect
      {|
    5
    10
    -10
    5
    32
    0
    20
    25
    0
    60
    30
    37
    37
    50
    |}]
  ;;

  let%expect_test "eval bool" =
    expect_eval "true;";
    expect_eval "false;";
    expect_eval "!false;";
    expect_eval "!!true;";
    expect_eval "!!5;";
    expect_eval "!!!5;";
    expect_eval "1 < 2;";
    expect_eval "1 > 2;";
    expect_eval "1 > 1;";
    expect_eval "1 == 1;";
    expect_eval "1 != 1;";
    expect_eval "1 == 2;";
    expect_eval "1 != 2;";
    expect_eval "true == true";
    expect_eval "false == false";
    expect_eval "true == false";
    expect_eval "true != false";
    expect_eval "(1 < 2) == true";
    expect_eval "(1 < 2) == false";
    expect_eval "(1 > 2) == false";
    expect_eval "(1 > 2) == true";
    [%expect
      {|
    true
    false
    true
    true
    true
    false
    true
    false
    false
    true
    false
    false
    true
    true
    true
    false
    true
    true
    false
    true
    false
      |}]
  ;;

  let%expect_test "eval conditional" =
    expect_eval "if (true) {10}";
    expect_eval "if (false) {10}";
    expect_eval "if (1) {10}";
    expect_eval "if (1 < 2) {10}";
    expect_eval "if (1 > 2) {10}";
    expect_eval "if (1 > 2) {10} else { 20 }";
    expect_eval "if (1 < 2) {10} else { 20 }";
    [%expect {|
    10
    null
    10
    10
    null
    20
    10
      |}]
  ;;

  let%expect_test "eval return statement" =
    expect_eval "return 10";
    expect_eval "return 10; 9;";
    expect_eval "return 2 * 5; 9";
    expect_eval "9; return 2 * 5; 9;";
    expect_eval "if (10 > 1) { if (10 > 1) { return 10; } return 1;}";
    [%expect {|
    10
    10
    10
    10
    10
      |}]
  ;;

  let%expect_test "error handling" =
    expect_err "5 + true;";
    expect_err "5 + true;5;";
    expect_err "-true";
    expect_err "true + false";
    expect_err "5; true + false; 5";
    expect_err "if (10 > 1) { true + false; }";
    expect_err
      {|
    if (10 > 1) {
      if (10 > 1) {
        return true + false;
      }
      return 1;
    }|};
    [%expect
      {|
  ERROR: type mismatch: INTEGER + BOOLEAN
  ERROR: type mismatch: INTEGER + BOOLEAN
  ERROR: unknown operator: -BOOLEAN
  ERROR: unknow operator: BOOLEAN + BOOLEAN
  ERROR: unknow operator: BOOLEAN + BOOLEAN
  ERROR: unknow operator: BOOLEAN + BOOLEAN
  ERROR: unknow operator: BOOLEAN + BOOLEAN
      |}]
  ;;
end
