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
val start_positive_offset : int
val start_negative_offset : int
val the_outer_scope : Types.scope
val no_entry : Identifier.id -> Types.entry
val currentScope : Types.scope ref
val quadNext : int ref
val tempNumber : int ref
val tab : Types.entry H.t ref
val initSymbolTable : int -> unit
val openScope : unit -> unit
val closeScope : unit -> unit
val hideScope : Types.scope -> bool -> unit
exception Failure_NewEntry of Types.entry
val newEntry : H.key -> Types.entry_info -> bool -> Types.entry
val lookupEntry : H.key -> Types.lookup_type -> bool -> Types.entry
val newVariable : H.key -> Types.typ -> bool -> Types.entry
val newUdt : H.key -> bool -> Types.entry
val newConstructor :
  H.key -> Types.typ -> Types.typ list -> bool -> Types.entry
val newFunction : H.key -> bool -> Types.entry
val newParameter :
  H.key -> Types.typ -> Types.pass_mode -> Types.entry -> bool -> Types.entry
val newTemporary : Types.typ -> Types.entry
val forwardFunction : Types.entry -> unit
val endFunctionHeader : Types.entry -> Types.typ -> unit
val setType : Types.entry -> Types.typ -> unit
val getType : Types.entry -> Types.typ
