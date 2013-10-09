open Error
open Pretty_print

(* Argument parsing *)

type config = {
  mutable in_file : string option;
  mutable quads : bool;
  mutable opt : bool;
}

(* Default configuration is
 * read from stdin
 * emit assembly
 * no optimisations *)
let default_config = {
  in_file = None;
  quads = false;
  opt = false;
} 

let open_files () =
  let out_ext = match default_config.quads with
    | true -> ".qua"
    | false -> ".asm"
  in
    match default_config.in_file with
      | Some name ->
        let (cin, cout) =
          try 
            (open_in name, open_out ((Filename.chop_extension name) ^ out_ext))
          with
            | Invalid_argument _ ->
              error "Wrong file name. Extension must be .lla";
              exit 1
            | Sys_error _ ->
              error "The file could not be opened.";
              exit 1
        in
          (cin, cout)
      | None ->
        (stdin, open_out ("a" ^ out_ext))

let read_args () =
  let speclist =
    [("-i", Arg.Unit (fun () -> default_config.quads <- true), 
      "Emit intermediate code");
     ("-O", Arg.Unit (fun () -> default_config.opt <- true),
      "Perform optimizations")]
  in
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-i] [-o] [infile]" in
    Arg.parse speclist (fun s -> default_config.in_file <- Some s) usage

let main =
  let () = read_args () in
  let (cin, cout) = open_files () in
  let lexbuf = Lexing.from_channel cin in
    try
      let ast = Parser.program Lexer.lexer lexbuf in
      let (solved, outer_entry, library_funs) = Ast.walk_program ast in
      let intermediate = Intermediate.gen_program ast solved outer_entry in
      let blocks = Cfg.Blocks.create_blocks intermediate in
      let () = Cfg.Blocks.print_blocks blocks in
      let cfg = Cfg.CFG.create_cfg intermediate in
      let () =  match default_config.quads with
        | true -> 
          Quads.printQuads (Format.formatter_of_out_channel cout) intermediate
        | false -> 
          let final = CodeGen.codeGen intermediate outer_entry in
          let asm = EmitMasm.emit final library_funs in
            Printf.fprintf cout "%s" asm;
      in
        exit 0
    with 
      | Parsing.Parse_error ->
        Printf.eprintf "Line %d: syntax error\n"
          (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum);
        exit 1
      | Typeinf.UnifyError (typ1, typ2) ->
        error "Cannot match type %a with type %a" 
          pretty_typ typ1 pretty_typ typ2;
        exit 2
      | Typeinf.TypeError (err, typ) ->
        error "Type error on type %a:\n %s" pretty_typ typ err;
        exit 2
      | Typeinf.DimError (dim1, dim2) ->
        error "Array dimensions error. Cannot match dimension size %a with %a" 
          pretty_dim dim1 pretty_dim dim2; 
        exit 2
      | Intermediate.InvalidCompare typ ->
        error "Cannot compare values of type %a" pretty_typ typ;
        exit 2

