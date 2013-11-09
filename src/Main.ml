open Error
open Pretty_print
open Lazy

(* Argument parsing *)

type config = {
  mutable in_file : string option;
  mutable quads   : bool;
  mutable opt     : bool;
  mutable cfg     : bool
}

type files = {
  cin    : Pervasives.in_channel;
  cout   : Pervasives.out_channel;
  cgraph : Pervasives.out_channel lazy_t
}

(* Default configuration is
 * read from stdin
 * emit assembly
 * no optimisations *)
let default_config = {
  in_file = None;
  quads = false;
  opt = false;
  cfg = false
} 


let open_files () =
  let out_ext = match default_config.quads with
    | true -> ".qua"
    | false -> ".asm"
  in
    match default_config.in_file with
      | Some name ->
        let files =
          try
            let chopped = Filename.chop_extension name in
              { cin = open_in name;
                cout = open_out (chopped ^ out_ext);
                cgraph = lazy (open_out_bin (chopped ^ ".dot"))
              }
          with
            | Invalid_argument _ ->
              error "Wrong file name. Extension must be .lla";
              exit 1
            | Sys_error _ ->
              error "The file could not be opened.";
              exit 1
        in
          files
      | None ->
        { cin = stdin; 
          cout = open_out ("a" ^ out_ext);
          cgraph = lazy (open_out_bin "a.dot")
        }


let read_args () =
  let speclist =
    [("-i", Arg.Unit (fun () -> default_config.quads <- true), 
      "Emit intermediate code");
     ("-O", Arg.Unit (fun () -> default_config.opt <- true; 
                                Quads.tailRecOpt := true),
      "Perform optimizations");
     ("-g", Arg.Unit (fun () -> default_config.cfg <- true),
      "Output a cfg in .dot format")]
  in
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-i] [-o] [-g] [infile]" in
    Arg.parse speclist (fun s -> default_config.in_file <- Some s) usage

let main =
  let () = read_args () in
  let files = open_files () in
  let lexbuf = Lexing.from_channel files.cin in
    try
      let ast = Parser.program Lexer.lexer lexbuf in
      let (solved, outer_entry, library_funs) = Ast.walk_program ast in
      let ir = Intermediate.gen_program ast solved outer_entry in
      let cfg = Cfg.CFG.create_cfg ir in
      let cfg = match default_config.opt with
          true -> 
          let optimized_cfg = Optimizations.optimize cfg in
            optimized_cfg
        | false -> cfg
      in
        if default_config.cfg then 
          Cfg.Dot.output_graph (force files.cgraph) cfg;
        let ir = Cfg.CFG.quads_of_cfg cfg in
        let () =  match default_config.quads with
          | true -> 
            Quads.printQuads (Format.formatter_of_out_channel files.cout) ir
          | false -> 
            let final = CodeGen.codeGen ir outer_entry in
            let final = match default_config.opt with
                true ->
                  Peephole.optimize final
              | false -> 
                  final
            in
            let asm = EmitMasm.emit final library_funs in
              Printf.fprintf files.cout "%s" asm;
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
      | Typeinf.DimAccesError (dim1, dim2) ->
        error "Array dimensions error. Cannot match dimension size %d with %d" 
          dim1 dim2;
      | Typeinf.DimSizeError (dim1, dim2) ->
        error "You are requesting the size of the #%d dimension in an array \
               with only %d dimensions" dim1 dim2;
        exit 2
      | Typeinf.UnsolvedTyVar tvar ->
          error "Unsolved type variable, \
            possibly arising from an unsed polymphic type"
      | Typeinf.UnsolvedDimVar tvar ->
          error "Unsolved dimension variable, possibly arising from \
            the use of an array with unspecified dimensions"
      | Intermediate.InvalidCompare typ ->
        error "Cannot compare values of type %a" pretty_typ typ;
        exit 2
      | Ast.RecDef typ ->
        error "%s connot be recursive" typ;
        exit 2
      | Ast.ConstrParamTyp (cid, tid) ->
        error "Constructor %s has one undefined argument, namely %s" cid tid;    
        exit 2
      | Ast.ConstrParamArity (cid, expected, got) ->
        error "The constructor %s expects %n argument(s),\
               but is applied here to %n argument(s)" cid expected got;
        exit 2
      | Ast.NewArray ->
        error "Cannot allocate array with new";
        exit 2

