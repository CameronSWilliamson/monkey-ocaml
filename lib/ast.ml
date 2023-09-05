type node =
  | Statement of statement
  | Program of program

and program = { statements: statement list }

and statement =
  | LetStatement of
      { name : identifier
      ; value : expression
      }

and identifier = { identifier : string }
and expression = SomeExpr

