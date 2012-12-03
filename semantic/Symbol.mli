module H :
  sig
    type key = Identifier.id
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
  end
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE
type param_status = PARDEF_COMPLETE | PARDEF_DEFINE | PARDEF_CHECK
type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool;
}
and variable_info = {
  mutable variable_type : Types.typ;
  variable_offset : int;
}
and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result : Types.typ;
  mutable function_pstatus : param_status;
  mutable function_initquad : int;
}
and parameter_info = {
  mutable parameter_type : Types.typ;
  mutable parameter_offset : int;
  parameter_mode : pass_mode;
}
and temporary_info = { temporary_type : Types.typ; temporary_offset : int; }
and constructor_info = {
  constructor_type : Types.typ;
  constructor_paramlist : Types.typ list;
}
and entry_info =
    ENTRY_none
  | ENTRY_variable of variable_info
  | ENTRY_function of function_info
  | ENTRY_parameter of parameter_info
  | ENTRY_temporary of temporary_info
  | ENTRY_udt
  | ENTRY_constructor of constructor_info
and entry = {
  entry_id : Identifier.id;
  entry_scope : scope;
  entry_info : entry_info;
}
type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES
val start_positive_offset : int
val start_negative_offset : int
val the_outer_scope : scope
val no_entry : Identifier.id -> entry
val currentScope : scope ref
val quadNext : int ref
val tempNumber : int ref
val tab : entry H.t ref
val initSymbolTable : int -> unit
val openScope : unit -> unit
val closeScope : unit -> unit
val hideScope : scope -> bool -> unit
exception Failure_NewEntry of entry
val newEntry : H.key -> entry_info -> bool -> entry
val lookupEntry : H.key -> lookup_type -> bool -> entry
val newVariable : H.key -> Types.typ -> bool -> entry
val newUdt : H.key -> bool -> entry
val newConstructor : H.key -> Types.typ -> Types.typ list -> bool -> entry
val newFunction : H.key -> bool -> entry
val newParameter : H.key -> Types.typ -> pass_mode -> entry -> bool -> entry
val newTemporary : Types.typ -> entry
val forwardFunction : entry -> unit
val endFunctionHeader : entry -> Types.typ -> unit
val setType : entry -> Types.typ -> unit
val getType : entry -> Types.typ
