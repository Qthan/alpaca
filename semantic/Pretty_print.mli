val pretty_dim : Format.formatter -> Types.dim -> unit
val pretty_typ : Format.formatter -> Types.typ -> unit
val pretty_mode : Format.formatter -> SymbTypes.pass_mode -> unit
val print_solved : (Types.typ * Types.typ) list -> unit
