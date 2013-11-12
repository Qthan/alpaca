(** Creates the text of the assembly generated, targeting MASM 5-6 *)

val library_funs : (string * Types.typ * (string * Types.typ) list) list ref
val regToString : Final.register -> string
val sizeToString : Final.size -> string
val operandToString : Final.operand -> string
val constFloatToString : float -> int -> string
val constStringToString : string -> int -> string
val libFunDecl : (string * 'a * 'b) list -> (string * 'c) list -> string
val instructionToString : Final.instruction -> string
val emit :
  Final.instruction list ->
  (string * Types.typ * (string * Types.typ) list) list -> string
