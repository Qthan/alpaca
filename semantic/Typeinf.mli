exception Unify of Types.typ * Types.typ
val debug_typeinf : bool
val print_constraints : (Types.typ * Types.typ) list -> unit
val add_solved_table : ('a * 'b) list -> ('a, 'b) Hashtbl.t -> unit
val fresh : unit -> Types.typ
val freshDim : unit -> Types.dim
val refresh : Types.typ -> Types.typ
val notIn : Types.typ -> Types.typ -> bool
val singleSub : Types.typ -> Types.typ -> Types.typ -> Types.typ
val subc :
  Types.typ ->
  Types.typ -> (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
val singleSubDim : Types.dim -> Types.dim -> Types.dim -> Types.dim
val subDim :
  Types.dim ->
  Types.dim -> (Types.dim * Types.dim) list -> (Types.dim * Types.dim) list
val singleSubArray : Types.dim -> Types.dim -> Types.typ -> Types.typ
val subArray :
  Types.dim ->
  Types.dim -> (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
val equalsType : Types.typ -> Types.typ -> bool
val unify : (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
