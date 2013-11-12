(** Generates the Intermediate Representation of Quads *)

exception InvalidCompare of Types.typ
val debug_quads : bool
val lookup_type : SymbTypes.entry option -> Types.typ
val lookup_res_type : SymbTypes.entry option -> Types.typ
val update_entry_typ : SymbTypes.entry -> unit
val update_def_typ : AstTypes.ast_def_node -> unit
val is_fun : AstTypes.ast_def_node -> bool
val split_decls : AstTypes.ast_stmt -> AstTypes.ast_stmt * AstTypes.ast_stmt
val isUnit : Types.typ -> bool
val failAtRuntime : string -> Quads.quad list -> Quads.quad list
val fun_stack : SymbTypes.entry Stack.t
val isTail : AstTypes.ast_expr_node -> bool
val gen_program :
  AstTypes.ast_stmt list -> 'a -> SymbTypes.entry -> Quads.quad list
val gen_decl_list :
  AstTypes.ast_stmt list -> SymbTypes.entry -> Quads.quad list
val gen_decl :
  Quads.quad list ->
  AstTypes.ast_stmt -> Quads.quad list ref -> Quads.quad list
val gen_type_eq : string -> Quads.quad list -> Quads.quad list
val gen_def_list :
  Quads.quad list ->
  AstTypes.ast_def_node list -> Quads.quad list ref -> Quads.quad list
val gen_def :
  Quads.quad list ->
  AstTypes.ast_def_node -> Quads.quad list ref -> Quads.quad list
val gen_expr :
  Quads.quad list ->
  AstTypes.ast_expr_node -> Quads.quad list * Quads.expr_info
val gen_cond :
  Quads.quad list ->
  AstTypes.ast_expr_node -> Quads.quad list * Quads.cond_info
val gen_stmt :
  Quads.quad list ->
  AstTypes.ast_expr_node -> Quads.quad list * Quads.stmt_info
val gen_atom :
  Quads.quad list ->
  AstTypes.ast_atom_node -> Quads.quad list * Quads.expr_info
val gen_atom_stmt :
  Quads.quad list ->
  AstTypes.ast_atom_node -> Quads.quad list * Quads.stmt_info
val gen_pattern :
  AstTypes.ast_pattern_node ->
  Quads.quad_operands -> Quads.quad list -> Quads.quad list * Quads.cond_info
val gen_pattom :
  AstTypes.ast_pattom_node ->
  Quads.quad_operands -> Quads.quad list -> Quads.quad list * Quads.cond_info
