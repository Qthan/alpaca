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
val debug_symbol : bool
val start_positive_offset : int
val start_negative_offset : int
val the_outer_scope : SymbTypes.scope
val no_entry : Identifier.id -> SymbTypes.entry
val currentScope : SymbTypes.scope ref
val quadNext : int ref
val tempNumber : int ref
val varNumber : int ref
val parNumber : int ref
val fun_index : int ref
val tab : SymbTypes.entry H.t ref
val initSymbolTable : int -> unit
val udt_table : SymbTypes.entry H.t
val addUdt : H.key -> SymbTypes.entry -> unit
val lookupUdt : H.key -> SymbTypes.entry
val show_offsets : bool
val pretty_mode : Format.formatter -> SymbTypes.pass_mode -> unit
val printSymbolTable : unit -> unit
val printState : string -> unit
val openScope : unit -> unit
val closeScope : unit -> unit
val hideScope : SymbTypes.scope -> bool -> unit
exception Failure_NewEntry of SymbTypes.entry
val newEntry : H.key -> SymbTypes.entry_info -> bool -> SymbTypes.entry
val lookupEntry : H.key -> SymbTypes.lookup_type -> bool -> SymbTypes.entry
val newParameter :
  H.key ->
  Types.typ ->
  SymbTypes.pass_mode -> SymbTypes.entry -> bool -> SymbTypes.entry
val newVariable :
  H.key -> Types.typ -> SymbTypes.entry -> bool -> SymbTypes.entry
val newUdt : H.key -> bool -> SymbTypes.entry
val addConstructor : SymbTypes.entry -> SymbTypes.entry -> unit
val getConstructors : SymbTypes.entry -> SymbTypes.entry list
val newConstructor :
  H.key -> Types.typ -> Types.typ list -> int -> bool -> SymbTypes.entry
val newFunction : H.key -> SymbTypes.entry option -> bool -> SymbTypes.entry
val newTemporary : Types.typ -> SymbTypes.entry
val forwardFunction : SymbTypes.entry -> unit
val endFunctionHeader : SymbTypes.entry -> Types.typ -> unit
val entry_eq : SymbTypes.entry -> SymbTypes.entry -> bool
val scoped_eq : SymbTypes.entry -> SymbTypes.entry -> bool
val setType : SymbTypes.entry -> Types.typ -> unit
val getType : SymbTypes.entry -> Types.typ
val getResType : SymbTypes.entry -> Types.typ
val setOffset : SymbTypes.entry -> int -> unit
val getOffset : SymbTypes.entry -> int
val getParamList : SymbTypes.entry -> SymbTypes.entry list
val getConstructorParamList : SymbTypes.entry -> Types.typ list
val getVarList : SymbTypes.entry -> SymbTypes.entry list
val getVarSize : SymbTypes.entry -> int
val getVarRef : SymbTypes.entry -> int ref
val fixVarOffsets : SymbTypes.function_info -> unit
val fixTmpOffsets : SymbTypes.function_info -> unit
val fixOffsets : SymbTypes.entry -> unit
val addTemp : SymbTypes.entry -> SymbTypes.entry -> unit
val remove_temp : SymbTypes.entry -> SymbTypes.entry -> unit
val setLibraryFunction : SymbTypes.entry -> unit
val isLibraryFunction : SymbTypes.entry -> bool
val getTag : SymbTypes.entry -> int
val getEqFun : SymbTypes.entry -> SymbTypes.entry
val isTemporary : SymbTypes.entry -> bool
