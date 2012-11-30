val fresh : unit -> Types.typ
val refresh : Types.typ -> Types.typ
val notIn : Types.typ -> Types.typ -> bool
val singleSub : Types.typ -> Types.typ -> Types.typ -> Types.typ
val subc :
  Types.typ ->
  Types.typ -> (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
val unify : (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
val updateSymbol : (string * 'a) list -> (Types.typ * Types.typ) list -> unit
val updateSymbolRec :
  Types.ast_def list -> (Types.typ * Types.typ) list -> unit
