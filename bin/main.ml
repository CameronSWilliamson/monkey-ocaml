open Base
open Stdio
open Monkey

let prompt = ">> "

let rec run_repl () =
  let _ = Out_channel.fprintf stdout "%s %!" prompt in
  match In_channel.input_line stdin with
  | Some line ->
    let lexer = Lexer.init line in
    let parser = Parser.init lexer in
    (match Parser.parse parser with
     | Ok program -> Out_channel.fprintf stdout "%s%!" @@ Ast.string_of_node program
     | Error msg -> Out_channel.fprintf stdout "%s%!" msg);
    Out_channel.flush stdout;
    run_repl ()
  | None -> ()
;;

let () = run_repl ()
