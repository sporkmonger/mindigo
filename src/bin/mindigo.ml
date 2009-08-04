let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Grammar.main Lexer.token lexbuf in
        Ast.print_ast result; flush stdout
    done
  with Lexer.Eof ->
    exit 0
