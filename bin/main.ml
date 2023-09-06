open Base
open Stdio
open Monkey

let prompt = ">> "

let run_repl f =
  let rec run_repl' f =
    let _ = Out_channel.fprintf stdout "%s %!" prompt in
    match In_channel.input_line stdin with
    | Some line ->
      f line;
      Out_channel.fprintf stdout "%!";
      run_repl' f
    | None -> ()
  in
  run_repl' f
;;

let _run_lexer line =
  let lexer = Lexer.init line in
  let rec run_lexer' lexer =
    match Lexer.next_token lexer with
    | _, Token.Eof -> ()
    | lexer, other ->
      Out_channel.fprintf stdout "%s\n" @@ Token.string_of_token other;
      run_lexer' lexer
  in
  run_lexer' lexer
;;

let run_parser line =
  let parser = Parser.init @@ Lexer.init line in
  let program = Parser.parse parser in
  match program with
  | Ok program -> Ast.print_node program
  | Error msg -> Out_channel.fprintf stdout "%s\n" msg
;;

let () = run_repl run_parser
