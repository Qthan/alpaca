open Format
open Identifier
open Error
open Types
open Pretty_print
open SymbTypes
open Typeinf

exception DuplicateTypeDef of string

module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
  )

let debug_symbol = false

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
let varNumber = ref 0
let parNumber = ref 0
let fun_index = ref (-1)

let tab = ref (H.create 0)

let initSymbolTable size =
  tab := H.create size;
  currentScope := the_outer_scope

let udt_table = H.create 11

let addUdt id entry = 
  if (H.mem udt_table id) then
    raise (DuplicateTypeDef (id_name id))
  else
    H.add udt_table id entry 
    
let lookupUdt id = H.find udt_table id

(* Functions for debugging symbol table *)

let show_offsets = true

let pretty_mode ppf mode =
  match mode with
    | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
    | _ -> ()

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
                  fprintf ppf "[%d] :%a" 
                    inf.variable_offset pretty_typ inf.variable_type
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
                let local ppf e =
                  match e.entry_info with
                    | ENTRY_variable inf ->
                      fprintf ppf "%a: %a"
                        pretty_id e.entry_id
                        pretty_typ inf.variable_type
                    | _ ->
                      fprintf ppf "<invalid>" in
                let rec locals ppf ls =
                  match ls with
                    | [l] ->
                      fprintf ppf "%a" local l
                    | l :: ls ->
                      fprintf ppf "%a; %a" local l locals ls;
                    | [] -> ()
                in
                  fprintf ppf "(%a) : %a - locals : %a"
                    params inf.function_paramlist
                    pretty_typ inf.function_result
                    locals inf.function_varlist
              | ENTRY_parameter inf ->
                if show_offsets then
                  fprintf ppf "[%d]" inf.parameter_offset
              | ENTRY_temporary inf ->
                if show_offsets then
                  fprintf ppf "[%d]" inf.temporary_offset
              | ENTRY_udt _ -> ()
              | ENTRY_constructor inf ->
                let pp_list ppf l = 
                  List.iter (fprintf ppf "%a " pretty_typ) l 
                in
                  fprintf ppf " Type: %a Parameters: %a" 
                    pretty_typ inf.constructor_type 
                    pp_list inf.constructor_paramlist
          end
      in
      let rec entries ppf es =
        match es with
          | [e] ->
            fprintf ppf "%a" entry e
          | e :: es ->
            fprintf ppf "%a \n %a" entry e entries es;
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

let newParameter id typ mode f err =
  match f.entry_info with
    | ENTRY_function fn -> begin
        match fn.function_pstatus with
          | PARDEF_DEFINE ->
            let inf = {
              parameter_type = typ;
              parameter_offset = 0;
              parameter_mode = mode;
              parameter_nesting = fn.function_nesting;
              parameter_index = (incr parNumber; !parNumber)
            } in
            let e = newEntry id (ENTRY_parameter inf) err in
              fn.function_paramlist <- e :: fn.function_paramlist;
              e
          | PARDEF_COMPLETE ->
            internal "Cannot add a parameter to an already defined function"
      end
    | _ ->
      internal "Cannot add a parameter to a non-function"

let newVariable id typ f err =
  match f.entry_info with
    | ENTRY_function fn ->
      let inf = {
        variable_type = typ;
        variable_offset = 0;
        variable_nesting = fn.function_nesting;
        variable_index = (incr varNumber; !varNumber)
      } in
      let e = newEntry id (ENTRY_variable inf) err in
        fn.function_varlist <- e :: fn.function_varlist;
        e
    | _ -> internal "Cannot add a variable to a non-function"



let newUdt id err =
  let typ = T_Id (id_name id) in 
  let outer_entry = lookupEntry (id_make "_outer") LOOKUP_ALL_SCOPES true in
  let param_info1 = {
    parameter_type = typ;
    parameter_offset = 8 + (sizeOfType typ);
    parameter_mode = PASS_BY_VALUE;
    parameter_nesting = 0;
    parameter_index = (incr parNumber; !parNumber)
  } in
  let param_info2 = {
    param_info1 with 
      parameter_offset = 8;
      parameter_index = (incr parNumber; !parNumber)
  } in
  let param_entry1 = {
    entry_id = id_make "a";
    entry_scope = !currentScope;
    entry_info = ENTRY_parameter param_info1
  } in
  let param_entry2 = {
    entry_id = id_make "b";
    entry_scope = !currentScope;
    entry_info = ENTRY_parameter param_info2
  } in
  let fun_inf = {
    function_isForward = false;
    function_paramlist = [param_entry1; param_entry2];
    function_varlist = [];
    function_tmplist = [];
    function_result = T_Bool;
    function_pstatus = PARDEF_DEFINE;
    function_varsize = ref 0;
    function_localsize = 0; 
    function_paramsize = 2*(sizeOfType typ);
    function_nesting = 0;
    function_parent = Some outer_entry;
    function_index = (incr fun_index; !fun_index);
    function_library = false;
    function_label = -1
  } in
  let fun_entry = {
    entry_id = id_make ("_eq" ^ (id_name id));
    entry_scope = !currentScope;
    entry_info = ENTRY_function fun_inf
  } in
  let inf = {
    udt_constructors = [];
    eq_function = fun_entry 
  } in
  let p = newEntry id (ENTRY_udt inf) err in
  let () = addUdt id p in
    p

let addConstructor entry c_entry =
  match entry.entry_info with
    | ENTRY_udt u -> u.udt_constructors <- c_entry :: u.udt_constructors
    | _ -> internal "Constructors can only be added to udts"

let getConstructors entry =
  match entry.entry_info with
    | ENTRY_udt u -> u.udt_constructors
    | _ -> internal "Constructors are only available to udts"

let newConstructor id typ typ_list tag err =
  let inf = {
    constructor_type = typ;
    constructor_paramlist = typ_list;
    constructor_tag = tag;
    constructor_arity = List.length typ_list
  } in
  let c = newEntry id (ENTRY_constructor inf) err in
  let tid = match typ with
    | T_Id tid -> tid
    | _ -> internal "not a udt"
  in
  let u_entry = lookupEntry (id_make tid) LOOKUP_CURRENT_SCOPE true in
  let () = addConstructor u_entry c in
    c

let newFunction id parent err =
  try
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in
      match e.entry_info with
        | ENTRY_function inf when inf.function_isForward ->
          inf.function_isForward <- false;
          e
        | _ ->
          if err then
            error "duplicate identifier: %a" pretty_id id;
          raise Exit
  with Not_found ->
    let nesting =
      match parent with
        | None -> -1
        | Some parent ->
          (match parent.entry_info with
            | ENTRY_function p -> p.function_nesting + 1
            | _ -> internal "Parent is not a function"
          )
    in
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_varlist = [];
      function_tmplist = [];
      function_result = T_Notype;
      function_pstatus = PARDEF_DEFINE;
      function_varsize = ref 0; 
      function_paramsize = 0;
      function_localsize = 0;
      function_nesting = nesting;
      function_parent = parent;
      function_index = (incr fun_index; !fun_index);
      function_library = false;
      function_label = -1;
    } in
      newEntry id (ENTRY_function inf) false

(* I don't think we are using this
 * we have one in Quads, which we should probably transfer here and delete this
 * one*)
let newTemporary typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
    !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
    let inf = {
      temporary_type = typ;
      temporary_offset = !currentScope.sco_negofs;
      temporary_index = !tempNumber;
      temporary_opt = false
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
            inf.function_paramlist <- List.rev inf.function_paramlist
      end;
      inf.function_pstatus <- PARDEF_COMPLETE
    | _ ->
      internal "Cannot end parameters in a non-function"

let entry_eq e1 e2 =
  match e1.entry_info, e2.entry_info with
    | ENTRY_function f1, ENTRY_function f2 ->
      f1.function_index = f2.function_index
    | ENTRY_variable v1, ENTRY_variable v2 ->
      v1.variable_index = v2.variable_index
    | ENTRY_parameter p1, ENTRY_parameter p2 ->
      p1.parameter_index = p2.parameter_index
    | ENTRY_temporary t1, ENTRY_temporary t2 ->
      t1.temporary_index = t2.temporary_index
    | x, y -> x = y

let scoped_eq e1 e2 = e1.entry_id = e2.entry_id

let setType entry typ = match entry.entry_info with
  | ENTRY_function f -> f.function_result <- typ
  | ENTRY_variable v -> v.variable_type <- typ
  | ENTRY_parameter p -> p.parameter_type <- typ
  | _ -> internal "Cannot update type."

let getType e = 
  match e.entry_info with
    | ENTRY_variable v -> v.variable_type
    | ENTRY_function f -> 
      let params = f.function_paramlist in
      let res_typ = f.function_result in
      let param_typ p = match p.entry_info with
        | ENTRY_parameter p -> p.parameter_type
        | _ -> internal "Formal parameters must be parameter entries"
      in
      let t = 
        List.fold_right (fun param ty -> T_Arrow (param_typ param, ty)) 
          params res_typ 
      in
        t
    | ENTRY_parameter p -> p.parameter_type
    | ENTRY_temporary t -> t.temporary_type
    | ENTRY_udt _ -> T_Id (id_name e.entry_id)
    | ENTRY_constructor c -> c.constructor_type
    | ENTRY_none -> internal "Invalid entry %s\n" (id_name e.entry_id)

let getResType e = 
  match e.entry_info with 
    | ENTRY_function f -> f.function_result 
    | _ -> getType e

let setOffset e offset = 
  match e.entry_info with
    | ENTRY_variable v -> v.variable_offset <-  offset
    | ENTRY_parameter p -> p.parameter_offset <- offset
    | ENTRY_temporary t -> t.temporary_offset <- offset
    | _ -> internal "cannot fix offset in a non variable or parameter entry"

let getOffset e = 
  match e.entry_info with 
    | ENTRY_variable v -> v.variable_offset
    | ENTRY_parameter p -> p.parameter_offset
    | ENTRY_temporary t -> t.temporary_offset
    | _ -> internal "cannot get offset in a non variable or parameter entry"


let getParamList e =
  match e.entry_info with
    | ENTRY_function f -> f.function_paramlist
    | _ -> internal "cannot find parameters in a non function"

let getConstructorParamList e =
  match e.entry_info with
    | ENTRY_constructor c -> c.constructor_paramlist
    | _ -> internal "cannot find parameters in a non constructor"

let getVarList e =
  match e.entry_info with
    | ENTRY_function f -> f.function_varlist
    | _ -> internal "cannot find variables in a non function"

let getVarSize e = 
  match e.entry_info with
    | ENTRY_function f -> !(f.function_varsize)
    | _ -> internal "Not a function"

(* Function's local variables size*)
let getVarRef entry =
  match entry.entry_info with
    | ENTRY_function f -> f.function_varsize
    | _ -> internal 
             "Looked for local variables of - Some thing - that's not a function"

let fixVarOffsets f =
  let rec aux varlist acc = 
    match varlist with 
      | [] -> 
        f.function_varsize <- ref (-acc);
        f.function_localsize <- -acc
      | v :: vs ->
        let s = sizeOfType (lookup_solved (getType v)) in
          setOffset v (acc-s);
          aux vs (acc-s)
  in
    aux f.function_varlist 0

let fixTmpOffsets f =
  let rec aux tmplist acc = 
    match tmplist with 
      | [] -> 
        f.function_varsize <- ref (-acc)
      | t :: ts ->
        let s = sizeOfType (lookup_solved (getType t)) in
          setOffset t (acc-s);
          aux ts (acc-s)
  in
    aux f.function_tmplist (-f.function_localsize)

let fixOffsets entry =
  let rec fixParamOffsets parlist acc =
    match parlist with 
      | [] -> acc
      | p :: ps ->
        let s = sizeOfType (lookup_solved (getType p)) in
          setOffset p acc;
          fixParamOffsets ps (acc+s)
  in
    match entry.entry_info with
      | ENTRY_function f ->
        let par_size = 
          (fixParamOffsets (List.rev f.function_paramlist) ar_size) - ar_size 
        in
        let () = fixVarOffsets f in
          f.function_paramsize <- par_size
      | _ -> internal "cannot fix offsets in a non function"

let addTemp tmp e =
  match e.entry_info with
    | ENTRY_function f -> 
      f.function_tmplist <- tmp :: f.function_tmplist
    | _ -> internal "Cannot add new temp to non-function entry"

(* this is as inefficient as I could do*)
let removeTemp tmp_e e =
  match e.entry_info with
      ENTRY_function f ->
      let tmps = List.filter (fun e -> not (entry_eq tmp_e e)) 
          f.function_tmplist in
        f.function_tmplist <- tmps;
    | _ -> internal "doesn't apply to non functions %s" (id_name e.entry_id)

let setLibraryFunction e = 
  match e.entry_info with
    | ENTRY_function f -> f.function_library <- true
    | _ -> internal "Entry not a function"

let isLibraryFunction e = 
  match e.entry_info with
    | ENTRY_function f -> f.function_library
    | _ -> internal "Entry not a function"

let getFunctionLabel e =
  match e.entry_info with
    | ENTRY_function f -> f.function_label
    | _ -> internal "Entry not a function"

let setFunctionLabel e v =
  match e.entry_info with
    | ENTRY_function f -> f.function_label <- v
    | _ -> internal "Entry not a function"

(* Getters for UDT entries *)
let getTag e = 
  match e.entry_info with
    | ENTRY_constructor c -> c.constructor_tag
    | _ -> internal "Tag's are only supported by UDT"

let getEqFun u_entry = match u_entry.entry_info with
  | ENTRY_udt u -> u.eq_function
  | _ -> internal "Not a UDT"

let isTemporary e =
  match e.entry_info with
    | ENTRY_temporary _ -> true
    | _ -> false

let isOptTemp e =
  match e.entry_info with
      ENTRY_temporary t ->
      t.temporary_opt
    | _ -> internal "not a temporary" (*returning false here would perhaps
                                        let some bugs slip, so we'll play
                                        safe for now.*)

let auxil_funs =
  let makeEntry id size typ = { 
    entry_id = (id_make id);
    entry_scope = 
      {
        sco_parent = None;
        sco_nesting = 0;
        sco_entries = [];
        sco_negofs  = 0;
        sco_hidden = false;
      };
    entry_info = 
      ENTRY_function {
        function_isForward = false;
        function_paramlist = [];
        function_varlist = [];
        function_tmplist = [];
        function_varsize = ref 0;
        function_paramsize = size;
        function_localsize = 0;       
        function_result = typ;
        function_pstatus = PARDEF_COMPLETE;
        function_nesting = 0;
        function_parent = None;
        function_index = -1;
        function_library = true;
        function_label = -1
      } } 
  in
    [ ("_make_array", makeEntry "_make_array" 4 (T_Array (T_Unit, D_Dim 1)));
      ("_delete_array", makeEntry "_delete_array" 2 T_Unit);
      ("_new", makeEntry "_new" 2 (T_Ref T_Int));
      ("_dispose", makeEntry "_dispose" 2 T_Unit);
      ("_dummy", makeEntry "_dummy" 0 T_Unit);
      ("_pow", makeEntry "_pow" 20 T_Float)]

let findAuxilEntry id = List.assoc id auxil_funs

let isAuxilFun id =
  List.mem_assoc (Identifier.id_name id) auxil_funs
