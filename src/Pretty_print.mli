(** Pretty printer for Types *)

val pretty_dim : Format.formatter -> Types.dim -> unit
val pretty_typ : Format.formatter -> Types.typ -> unit
val print_solved : (Types.typ * Types.typ) list -> unit
