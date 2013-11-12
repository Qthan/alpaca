(** Walks the AST performing semantic and other checks *)

exception RecDef of string
exception ConstrParamTyp of string * string
exception ConstrParamArity of string * int * int
exception NewArray
val library_funs : (string * Types.typ * (string * Types.typ) list) list
val function_stack : SymbTypes.entry Stack.t
val library_fun_entries : (string * SymbTypes.entry) list ref
val find_lib_fun : string -> SymbTypes.entry
val walk_program :
  AstTypes.ast_stmt list ->
  (Types.typ * Types.typ) list * SymbTypes.entry *
  (string * Types.typ * (string * Types.typ) list) list
val insert_function :
  string * Types.typ * (string * Types.typ) list -> string * SymbTypes.entry
val walk_stmt_list : AstTypes.ast_stmt list -> (Types.typ * Types.typ) list
val walk_stmt : AstTypes.ast_stmt -> (Types.typ * Types.typ) list
val walk_def_list :
  AstTypes.ast_def_node list -> (Types.typ * Types.typ) list
val walk_recdef_list :
  AstTypes.ast_def_node list -> (Types.typ * Types.typ) list
val walk_def : AstTypes.ast_def_node -> (Types.typ * Types.typ) list
val walk_recdef_names : AstTypes.ast_def_node -> unit
val walk_recdef_params : AstTypes.ast_def_node -> unit
val walk_recdef : AstTypes.ast_def_node -> (Types.typ * Types.typ) list
val walk_par_list : (string * Types.typ) list -> SymbTypes.entry -> unit
val walk_typedef_list :
  (string * (string * Types.typ list) list) list ->
  (Types.typ * Types.typ) list
val walk_expr : AstTypes.ast_expr_node -> (Types.typ * Types.typ) list
val walk_atom : AstTypes.ast_atom_node -> (Types.typ * Types.typ) list
val walk_clause_list :
  AstTypes.ast_clause list ->
  Types.typ * Types.typ * (Types.typ * Types.typ) list
val walk_clause :
  AstTypes.ast_clause -> Types.typ * Types.typ * (Types.typ * Types.typ) list
val walk_pattern : AstTypes.ast_pattern_node -> (Types.typ * Types.typ) list
val walk_pattom : AstTypes.ast_pattom_node -> (Types.typ * Types.typ) list
