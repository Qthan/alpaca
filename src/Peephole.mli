(** Performs peephole optimizations on the final code *)
val peep3 :
  Final.instruction list -> Final.instruction list -> Final.instruction list
val peep2 :
  Final.instruction list -> Final.instruction list -> Final.instruction list
val peep1 :
  Final.instruction list -> Final.instruction list -> Final.instruction list
val optimize : Final.instruction list -> Final.instruction list
