exception TypeError of string * Types.typ
exception UnifyError of Types.typ * Types.typ
exception DimSizeError of int * int
exception DimAccesError of int * int
exception UnsolvedTyVar of Types.typ
val debug_typeinf : bool
val print_constraints : (Types.typ * Types.typ) list -> unit
val solved_types : (Types.typ, Types.typ) Hashtbl.t
val add_solved_table : (Types.typ * Types.typ) list -> unit
val lookup_solved : Types.typ -> Types.typ
val fresh : unit -> Types.typ
val dim_size : int -> Types.dim
val refresh : Types.typ -> Types.typ
val notIn : Types.typ -> Types.typ -> bool
val singleSub : Types.typ -> Types.typ -> Types.typ -> Types.typ
val subc :
  Types.typ ->
  Types.typ -> (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
val subl : Types.typ -> Types.typ -> Types.typ list -> Types.typ list
val unify : (Types.typ * Types.typ) list -> (Types.typ * Types.typ) list
