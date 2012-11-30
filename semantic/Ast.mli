val walk_program : Types.ast_stmt list -> unit
val walk_stmt_list : Types.ast_stmt list -> unit
val walk_stmt : Types.ast_stmt -> unit
val walk_def_list : Types.ast_def list -> unit
val walk_recdef_list : Types.ast_def list -> (Types.typ * Types.typ) list
val walk_def : Types.ast_def -> unit
val walk_recdef_names : Types.ast_def -> unit
val walk_recdef_params : Types.ast_def -> unit
val walk_recdef : Types.ast_def -> (Types.typ * Types.typ) list
val walk_par_list : (string * Types.typ) list -> Symbol.entry -> unit
val walk_expr : Types.ast_expr -> Types.typ * (Types.typ * Types.typ) list
val walk_atom_list : Types.ast_atom list -> unit
val walk_expr_list : Types.ast_expr list -> unit
val walk_atom : Types.ast_atom -> Types.typ * (Types.typ * Types.typ) list
val walk_clause_list :
  Types.ast_clause list ->
  (Types.typ * Types.typ * (Types.typ * Types.typ) list) list
val walk_clause :
  Types.ast_clause -> Types.typ * Types.typ * (Types.typ * Types.typ) list
val walk_pattern : Types.ast_pattern -> Types.typ
val walk_pattom : Types.ast_pattom -> Types.typ
val walk_pattom_list : Types.ast_pattom list -> unit
