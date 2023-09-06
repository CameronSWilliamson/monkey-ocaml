open Base

type node =
  | Statement of statement
  | Program of program
  | Expression of expression

and program = { statements : statement list }

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of { value : expression }
  | ExpressionStmt of expression
  | SomeStmt

and identifier = { identifier : string }

and expression =
  | Identifier of identifier
  | Integer of int
  | Prefix of
      { op : string
      ; right : expression
      }
  | Infix of
      { left : expression
      ; op : string
      ; right : expression
      }
  | SomeExpr
[@@deriving show { with_path = false }, sexp]

let string_of_identifier ident = ident.identifier

let rec string_of_expression = function
  | Identifier ident -> Fmt.str "%s" (string_of_identifier ident)
  | Integer num -> Fmt.str "%d" num
  | Prefix expr -> Fmt.str "(%s%s)" expr.op (string_of_expression expr.right)
  | Infix expr ->
    Fmt.str
      "(%s %s %s)"
      (string_of_expression expr.left)
      expr.op
      (string_of_expression expr.right)
  | SomeExpr -> Fmt.str "NOT IMPLEMENTED"
;;

let string_of_statment = function
  | Let stmt ->
    Fmt.str
      "LET: let %s = %s"
      (string_of_identifier stmt.name)
      (string_of_expression stmt.value)
  | Return stmt -> Fmt.str "RETURN: return %s" (string_of_expression stmt.value)
  | ExpressionStmt expr -> Fmt.str "EXPR: %s" (string_of_expression expr)
  | _ -> ""
;;

let print_node node =
  let program =
    match node with
    | Program prog -> prog
    | _ -> failwith "Expecting program"
  in
  Fmt.pr "Program: [@.";
  List.iter program.statements ~f:(fun stmt -> Fmt.pr "  %s@." (string_of_statment stmt));
  Fmt.pr "]"
;;
