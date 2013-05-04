open Identifier
open Error
open Types
open SymbTypes
open Pretty_print
open Printf
open Format

module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

let debug_symbol = true

(* Symbol table definitions *)

let start_positive_offset = 8
let start_negative_offset = 0

let the_outer_scope = {
  sco_parent = None;
  sco_nesting = 0;
  sco_entries = [];
  sco_negofs = start_negative_offset;
  sco_hidden = false
}

let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope
let quadNext = ref 1
let tempNumber = ref 1

let tab = ref (H.create 0)

let initSymbolTable size =
   tab := H.create size;
   currentScope := the_outer_scope

(* Functions for debugging symbol table *)

let show_offsets = true

let printSymbolTable () =
  let rec walk ppf scp =
    if scp.sco_nesting <> 0 then begin
      fprintf ppf "scope: ";
      let entry ppf e =
        if (scp.sco_hidden) then 
          fprintf ppf "Hidden!!"
        else
          begin
            fprintf ppf "%a" pretty_id e.entry_id;
            match e.entry_info with
              | ENTRY_none ->
                  fprintf ppf "<none>"
              | ENTRY_variable inf ->
                  if show_offsets then
                    fprintf ppf "[%d] :%a" inf.variable_offset pretty_typ inf.variable_type
              | ENTRY_function inf ->
                  let param ppf e =
                    match e.entry_info with
                      | ENTRY_parameter inf ->
                          fprintf ppf "%a%a : %a"
                            pretty_mode inf.parameter_mode
                            pretty_id e.entry_id
                            pretty_typ inf.parameter_type
                      | _ ->
                          fprintf ppf "<invalid>" in
                  let rec params ppf ps =
                    match ps with
                      | [p] ->
                          fprintf ppf "%a" param p
                      | p :: ps ->
                          fprintf ppf "%a; %a" param p params ps;
                      | [] ->
                          () in
                    fprintf ppf "(%a) : %a"
                      params inf.function_paramlist
                      pretty_typ inf.function_result
              | ENTRY_parameter inf ->
                  if show_offsets then
                    fprintf ppf "[%d]" inf.parameter_offset
              | ENTRY_temporary inf ->
                  if show_offsets then
                    fprintf ppf "[%d]" inf.temporary_offset
              | ENTRY_udt -> ()
              | ENTRY_constructor inf ->
                  let pp_list ppf l = List.iter (fprintf ppf "%a " pretty_typ) l in
                  fprintf ppf " Type: %a Parameters: %a" pretty_typ inf.constructor_type pp_list inf.constructor_paramlist
          end
      in
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
        match scp.sco_parent with
          | Some scpar ->
              fprintf ppf "%a\n%a"
                entries scp.sco_entries
                walk scpar
          | None ->
              fprintf ppf "<impossible>\n"
    end 
  in
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      walk ppf scp in
    printf "%a----------------------------------------\n"
      scope !currentScope

let printState msg =
  Printf.printf "%s:\n" msg;
  printSymbolTable ()
                     
(* Symbol table functions *) 

let openScope () =
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset;
    sco_hidden = false
  } in
  currentScope := sco;
  if (debug_symbol) then printState "Opened scope"

let closeScope () =
  let sco = !currentScope in
  let manyentry e = H.remove !tab e.entry_id in
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
      currentScope := scp;
      if (debug_symbol) then printState "Closed scope"
  | None ->
      internal "cannot close the outer scope!"

let hideScope sco flag =
  sco.sco_hidden <- flag;
  if (debug_symbol) then printState "Hidden scope"

exception Failure_NewEntry of entry

let newEntry id inf err =
  try
    if err then begin
      try
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
           raise (Failure_NewEntry e)
      with Not_found ->
        ()
    end;
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    H.add !tab id e;
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    if (debug_symbol) then printState "Added new entry";
    e
  with Failure_NewEntry e ->
    error "duplicate identifier %a" pretty_id id;
    e

let lookupEntry id how err =
  let scc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = scc.sco_nesting then
          e
        else
          raise Not_found
    | LOOKUP_ALL_SCOPES ->
        let rec walk es =
          match es with
          | [] ->
              raise Not_found
          | e :: es ->
              if not e.entry_scope.sco_hidden then
                e
              else
                walk es in
        walk (H.find_all !tab id) in
  if err then
    try
      lookup ()
    with Not_found ->
      error "unknown identifier %a (first occurrence)"
        pretty_id id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise Exit
  else
    lookup ()

let newVariable id typ err =
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs
  } in
  newEntry id (ENTRY_variable inf) err

let newUdt id err =
  newEntry id ENTRY_udt err

let newConstructor id typ typ_list err =
  let inf = {
    constructor_type = typ;
    constructor_paramlist = typ_list
  } in
    newEntry id (ENTRY_constructor inf) err  

let newFunction id err =
  try
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in
    match e.entry_info with
    | ENTRY_function inf when inf.function_isForward ->
        inf.function_isForward <- false;
        (* inf.function_pstatus <- PARDEF_CHECK; *)
        (* inf.function_redeflist <- inf.function_paramlist; *)
        e
    | _ ->
        if err then
          error "duplicate identifier: %a" pretty_id id;
          raise Exit
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = T_Notype;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0
    } in
    newEntry id (ENTRY_function inf) false

let newParameter id typ mode f err =
  match f.entry_info with
  | ENTRY_function inf -> begin
      match inf.function_pstatus with
      | PARDEF_DEFINE ->
          let inf_p = {
            parameter_type = typ;
            parameter_offset = 0;
            parameter_mode = mode
          } in
          let e = newEntry id (ENTRY_parameter inf_p) err in
          inf.function_paramlist <- e :: inf.function_paramlist;
          e
      | PARDEF_CHECK -> begin
          match inf.function_redeflist with
          | p :: ps -> begin
              inf.function_redeflist <- ps;
              match p.entry_info with
              | ENTRY_parameter inf ->
                  if not (equalType inf.parameter_type typ) then
                    error "Parameter type mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else if inf.parameter_mode != mode then
                    error "Parameter passing mode mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else if p.entry_id != id then
                    error "Parameter name mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else begin
                    H.add !tab id p;
                    !currentScope.sco_entries <- p :: !currentScope.sco_entries
                  end;
                  p
              | _ ->
                  internal "I found a parameter that is not a parameter!"
                  (* raise Exit *)
            end
          | [] ->
              error "More parameters than expected in redeclaration \
                     of function %a" pretty_id f.entry_id;
              raise Exit
        end
      | PARDEF_COMPLETE ->
          internal "Cannot add a parameter to an already defined function"
          (* raise Exit *)
    end
  | _ ->
      internal "Cannot add a parameter to a non-function"
      (* raise Exit *)

let newTemporary typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs
  } in
  incr tempNumber;
  newEntry id (ENTRY_temporary inf) false

let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
      inf.function_isForward <- true
  | _ ->
      internal "Cannot make a non-function forward"

let endFunctionHeader e typ =
  match e.entry_info with
  | ENTRY_function inf ->
      begin
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
            internal "Cannot end parameters in an already defined function"
        | PARDEF_DEFINE ->
            inf.function_result <- typ;
            let offset = ref start_positive_offset in
            let fix_offset e =
              match e.entry_info with
              | ENTRY_parameter inf ->
                  inf.parameter_offset <- !offset;
                  let size =
                    match inf.parameter_mode with
                    | PASS_BY_VALUE     -> sizeOfType inf.parameter_type
                    | PASS_BY_REFERENCE -> 2 in
                  offset := !offset + size
              | _ ->
                  internal "Cannot fix offset to a non parameter" in
            List.iter fix_offset inf.function_paramlist;
            inf.function_paramlist <- List.rev inf.function_paramlist
        | PARDEF_CHECK ->
            if inf.function_redeflist <> [] then
              error "Fewer parameters than expected in redeclaration \
                     of function %a" pretty_id e.entry_id;
            if not (equalType inf.function_result typ) then
              error "Result type mismatch in redeclaration of function %a"
                    pretty_id e.entry_id;
      end;
      inf.function_pstatus <- PARDEF_COMPLETE
  | _ ->
      internal "Cannot end parameters in a non-function"

let setType entry typ = match entry.entry_info with
  | ENTRY_function func_info -> func_info.function_result <- typ
  | _ -> ()

let getType entry = match entry.entry_info with
  | ENTRY_function func_info -> func_info.function_result
  | ENTRY_variable var -> var.variable_type
  | _ -> internal "not a function or var"
         (* raise Exit *)
