open Error
open Pretty_print


let main =
  let (cin, cout) =
    if Array.length Sys.argv >1
    then 
      try
        (open_in Sys.argv.(1), open_out ((Filename.chop_extension Sys.argv.(1))^ ".asm" ))
      with
        | Invalid_argument _ -> 
          error "Wrong file name. Extension must be .lla";
          exit 1
        | Sys_error _ ->
          error "The file could not be opened.";
          exit 1
    else (stdin, open_out "a.asm")
  in
  let lexbuf = Lexing.from_channel cin in
    try
      let ast = Parser.program Lexer.lexer lexbuf in
      let (solved, outer_entry, library_funs) = Ast.walk_program ast in
      let intermediate = Intermediate.gen_program ast solved outer_entry in
      let final = CodeGen.codeGen intermediate outer_entry in
        exit 0
    with 
      | Parsing.Parse_error ->
        Printf.eprintf "Line %d: syntax error\n"
          (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum);
        exit 1
      | Typeinf.UnifyError (typ1, typ2) ->
        error "Cannot match type %a with type %a" pretty_typ typ1 pretty_typ typ2;
        exit 2
      | Typeinf.TypeError (err, typ) ->
        error "Type error on type %a:\n %s" pretty_typ typ err;
        exit 2
      | Typeinf.DimError (dim1, dim2) ->
        error "Array dimensions error. Cannot match dimension size %a with %a" pretty_dim dim1 pretty_dim dim2; 
        exit 2
