(** Generation of machine code *)

val params_size : int ref
val debug_codeGen : bool ref
val codeGen : Quads.quad list -> SymbTypes.entry -> Final.instruction list
val quadToFinal :
  Quads.quad -> Final.instruction list -> Final.instruction list
