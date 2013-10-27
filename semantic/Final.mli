(** Infrastructure for creation of native code *)

(** available register types*)
type register =
    Ax
  | Bx
  | Cx
  | Dx
  | Di
  | Si
  | Bp
  | Sp
  | Al
  | Ah
  | Bl
  | Bh
  | Cl
  | Ch
  | Dl
  | Dh
  | ST of int
type size = Byte | Word | TByte | DWord | Near
type operand =
    Reg of register
  | Pointer of (size * operand * int)
  | LabelPtr of (size * string)
  | Label of string
  | Immediate of string
type instruction =
    Prelude of SymbTypes.entry
  | Epilogue
  | Mov of operand * operand
  | Lea of operand * operand
  | Add of operand * operand
  | Sub of operand * operand
  | Neg of operand
  | Imul of operand
  | Idiv of operand
  | Cmp of operand * operand
  | Cwd
  | And of operand * operand
  | Or of operand * operand
  | Xor of operand * operand
  | Not of operand
  | Test of operand * operand
  | Jmp of operand
  | CondJmp of string * operand
  | Push of operand
  | Pop of operand
  | Call of operand
  | Ret
  | Fld of operand
  | Faddp of operand * operand
  | Fsubp of operand * operand
  | Fmulp of operand * operand
  | Fdivp of operand * operand
  | Fcompp
  | Sahf
  | Fstsw of operand
  | Fstp of operand
  | Fun of string
  | EndFun of string
  | LabelDecl of string
  | Comment of string
  | Interrupt of operand
val word_size : int
val current_fun : SymbTypes.entry ref
val newInstrList : unit -> 'a list
val genInstr : 'a -> 'a list -> 'a list
val str_lst : (string * int) list ref
val strLabel : int -> string
val saveString : string -> string
val flt_lst : (float * int) list ref
val fltLabel : int -> string
val saveFloat : float -> string
val getNesting : SymbTypes.entry -> int
val getTypeSize : Types.typ -> size
val getSize : SymbTypes.entry -> size
val getRefSize : SymbTypes.entry -> size
val sizeToBytes : size -> string
val getRegister : register -> Types.typ -> register
val makeFunctionLabel : SymbTypes.entry -> string
val makeLabel : Quads.quad_operands -> string
val relOpJmp : Quads.quad_operators -> string
val relOpJmpF : Quads.quad_operators -> string
val functionResult : SymbTypes.entry -> Types.typ
val getAR : SymbTypes.entry -> instruction list -> instruction list
val load :
  register -> Quads.quad_operands -> instruction list -> instruction list
val loadAddress :
  register -> Quads.quad_operands -> instruction list -> instruction list
val loadReal : Quads.quad_operands -> instruction list -> instruction list
val store :
  register -> Quads.quad_operands -> instruction list -> instruction list
val storeReal : Quads.quad_operands -> instruction list -> instruction list
val loadFunEnv :
  register -> Quads.quad_operands -> instruction list -> instruction list
val loadFunCode :
  register -> Quads.quad_operands -> instruction list -> instruction list
val loadFun :
  register ->
  register -> Quads.quad_operands -> instruction list -> instruction list
val updateAL : Quads.quad_operands -> instruction list -> instruction list
val storeFun :
  register ->
  register -> Quads.quad_operands -> instruction list -> instruction list
