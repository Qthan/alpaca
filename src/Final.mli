(** Infrastructure for creation of native code *)

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
(** Available register types*)

type size = Byte | Word | TByte | DWord | Near
(** Available size types *) 

type operand =
    Reg of register
  | Pointer of (size * operand * int)
  | LabelPtr of (size * string)
  | Label of string
  | Immediate of string
(** Instruction operands *)                                            

type instruction =
  | Prelude of SymbTypes.entry
  | Epilogue
  | Mov of operand * operand
  | Lea of operand * operand
  | Add of operand * operand
  | Sub of operand * operand
  | Inc of operand
  | Dec of operand
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
(**  Available instruction types *)
                   
val word_size : int
(** CPU word size*)
val current_fun : SymbTypes.entry ref
(** A reference to the current function compiled start with a dummy reference. *)
val newInstrList : unit -> 'a list
(** An empty instruction list *)
val genInstr : 'a -> 'a list -> 'a list
(** A function adding a new instruction *)
val str_lst : (string * int) list ref
(** A list holding strings and their unique id *)
val strLabel : int -> string
val saveString : string -> string
(** A closure for saving new strings *)
val flt_lst : (float * int) list ref
(** A list holding float constants and their unique id*)
val fltLabel : int -> string
val saveFloat : float -> string
(** A closure for saving new floats *)
val getNesting : SymbTypes.entry -> int
(** A function returning the nesting level of an entry *)
val getTypeSize : Types.typ -> size
(** A function returning the size of a type *)
val getSize : SymbTypes.entry -> size
(** A function returning the size of an entry's type *)
val getRefSize : SymbTypes.entry -> size
(** A function returning the size of referenced object *)
val sizeToBytes : size -> string
(** A function returning the size in bytes *)
val getRegister : register -> Types.typ -> register
(** A function returning a suitable register according to the type size *)
val makeFunctionLabel : SymbTypes.entry -> string
(** A function returning the label of a function*) 
val makeLabel : Quads.quad_operands -> string
(** A function returning the label of a quad*)     
val relOpJmp : Quads.quad_operators -> string
(** A function returning the jump instruction *)
val relOpJmpF : Quads.quad_operators -> string
val revertCond : string -> string
val functionResult : SymbTypes.entry -> Types.typ
(** A function returning the result type of a function *)
val getAR : SymbTypes.entry -> instruction list -> instruction list
(** A function returning the activation record in which a var lives *)
val load :
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for loading a into register r *)
val loadAddress :
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for loading the address of a into register r*)
val loadReal : Quads.quad_operands -> instruction list -> instruction list
(** A function for loading float values into float stack *)
val store :
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for storing register's r contents into a *)
val storeReal : Quads.quad_operands -> instruction list -> instruction list
(** A function for storing float stack contents into a *)
val loadFunEnv :
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for loading a function's enviroment into register r *)
val loadFunCode :
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for loading a function's code into register r *)
val loadFun :
  register ->
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for loading a function's code into register r1  
  and enviroment into register r2 *)
val updateAL : Quads.quad_operands -> instruction list -> instruction list
(** A function for updating the active link of the callee *)    
val storeFun :
  register ->
  register -> Quads.quad_operands -> instruction list -> instruction list
(** A function for storing a function's code from register r1 
  and enviroment from register r2 into a*)

