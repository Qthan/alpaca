let main =
  let cin =
    if Array.length Sys.argv >1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
    try
      Parser.program Lexer.lexer lexbuf;
      exit 0
    with Parsing.Parse_error ->
      Printf.eprintf "syntax error\n";
      exit 1
