let main =
  let cin =
    if Array.length Sys.argv >1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  try
    let intermediate = Parser.program Lexer.lexer lexbuf in
    let final = CodeGen.codeGen intermediate in
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "Line %d: syntax error\n"
      (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum);
    exit 1
