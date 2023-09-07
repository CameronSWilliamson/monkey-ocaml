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
  | BlockStmt of block
  | SomeStmt

and identifier = { identifier : string }
and block = { block : statement list }

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
  | Boolean of bool
  | If of
      { condition : expression
      ; consequence : block
      ; alternative : block option
      }
  | FnLiteral of
      { parameters : identifier list
      ; body : block
      }
  | CallExpr of
      { func : expression
      ; arguments : expression list
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
  | Boolean boolean -> Fmt.str "%b" boolean
  | If expr ->
    Fmt.str
      "if (%s) %s %s"
      (string_of_expression expr.condition)
      (string_of_block expr.consequence)
      (match expr.alternative with
       | Some block -> string_of_block block
       | None -> "")
  | FnLiteral lit ->
    Fmt.str
      "fn(%s) %s"
      (List.fold lit.parameters ~init:"" ~f:(fun acc param ->
         Fmt.str "%s%s%s" acc (if String.length acc > 0 then ", " else "")
         @@ string_of_identifier param))
      (string_of_block lit.body)
  | CallExpr expr -> Fmt.str "%s (%s)" (string_of_expression expr.func)
      (List.fold expr.arguments ~init:"" ~f:(fun acc param ->
         Fmt.str "%s%s%s" acc (if String.length acc > 0 then ", " else "")
         @@ string_of_expression param))
  | _ -> "PRINT NOT IMPLEMENTED"

and string_of_block block =
  Fmt.str "BLOCK: [%s]" @@ string_of_statment_list ~s:" " ~d:";" block.block

and string_of_statment = function
  | Let stmt ->
    Fmt.str
      "LET: let %s = %s"
      (string_of_identifier stmt.name)
      (string_of_expression stmt.value)
  | Return stmt -> Fmt.str "RETURN: return %s" (string_of_expression stmt.value)
  | ExpressionStmt expr -> Fmt.str "EXPR: %s" (string_of_expression expr)
  | _ -> ""

and string_of_statment_list ?(s = "  ") ?(d = "\n") stmts =
  List.fold stmts ~init:"" ~f:(fun acc stmt ->
    if String.(d = "\n")
    then Fmt.str "%s%s%s@." acc s (string_of_statment stmt)
    else Fmt.str "%s%s%s%s" acc s (string_of_statment stmt) d)

and string_of_program program =
  let stmts = string_of_statment_list program.statements in
  Fmt.str "Program: [@.%s]@." stmts
;;

let print_node = function
  | Program program -> Fmt.pr "%s" @@ string_of_program program
  | _ -> failwith "Expecting program"
;;
