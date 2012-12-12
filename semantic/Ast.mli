val walk_program : Types.ast_stmt list -> unit
val walk_stmt_list : Types.ast_stmt list -> (Types.typ * Types.typ) list
val walk_stmt : Types.ast_stmt -> (Types.typ * Types.typ) list
val walk_def_list : Types.ast_def_node list -> (Types.typ * Types.typ) list
val walk_recdef_list :
  Types.ast_def_node list -> (Types.typ * Types.typ) list
val walk_def : Types.ast_def_node -> (Types.typ * Types.typ) list
val walk_recdef_names : Types.ast_def_node -> unit
val walk_recdef_params : Types.ast_def_node -> unit
val walk_recdef : Types.ast_def_node -> (Types.typ * Types.typ) list
val walk_par_list : (string * Types.typ) list -> Types.entry -> unit
val walk_typedef_list :
  (string * (string * Types.typ list) list) list ->
  (Types.typ * Types.typ) list
val walk_expr : Types.ast_expr_node -> (Types.typ * Types.typ) list
val walk_atom : Types.ast_atom_node -> (Types.typ * Types.typ) list
val walk_clause_list :
  Types.ast_clause list ->
  Types.typ * Types.typ * (Types.typ * Types.typ) list
val walk_clause :
  Types.ast_clause -> Types.typ * Types.typ * (Types.typ * Types.typ) list
val walk_pattern : Types.ast_pattern_node -> (Types.typ * Types.typ) list
val walk_pattom : Types.ast_pattom_node -> (Types.typ * Types.typ) list
