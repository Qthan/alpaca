
let regToString = function
  | Ax -> "ax"
  | Bx -> "bx"
  | Cx -> "cx"
  | Dx -> "dx"
  | Di -> "di"
  | Si -> "si"
  | Bp -> "bp"
  | Sp -> "sp"
  | Al -> "al"
  | Ah -> "ah"
  | Bl -> "bl"
  | Bh -> "bh"
  | Cl -> "cl"
  | Ch -> "ch"
  | Dl -> "dl"
  | Dh -> "dh"
  | ST i -> Printf.sprintf "ST(%d)" i

let sizeToString = function
  | Byte -> "byte"
  | Word -> "word"
  | TByte -> "tbyte"
  | DWord -> "dword"
  | Near -> "near"

let operandToString = function 
  | Reg r -> regToString r
  | Pointer (size, operand, i) when i = 0 -> 
    Printf.sprintf "%s ptr [%s]" (sizeToString size) (operandToString operand)
  | Pointer (size, operand, i) -> 
    Printf.sprintf "%s ptr [%s+%d]" (sizeToString size) (operandToString operand) i
  | LabelPtr (size, label) ->
    Printf.sprintf "%s ptr [%s]" (sizeToString size) label
  | Label l -> l
  | Immediate i -> i

let instructionToString = function
  | Prelude outer -> 
    "xseg segment public ′code′\n\
     \tassume cs : xseg, ds : xseg, ss : xseg\n\
     \torg\t100h\n\
     main proc\tnear\n\
     \tcall\tnear ptr " ^ (makeFunctionLabel outer) ^ "\n\
                                                      \tmov\tax, 4C00h\n\
                                                      \tint\t21h\n\
                                                      main endp\n"
  | Epilogue ->
    "xseg ends\n\
     \tend\tmain\n"
  | Mov (op1, op2) ->
    Printf.sprintf "\tmov %s, %s\n" (operandToString op1) (operandToString op2)
  | Lea (op1, op2) ->
    Printf.sprintf "\tlea %s, %s\n" (operandToString op1) (operandToString op2)
  | Add (op1, op2) ->
    Printf.sprintf "\tadd %s, %s\n" (operandToString op1) (operandToString op2)
  | Sub (op1, op2) ->
    Printf.sprintf "\tsub %s, %s\n" (operandToString op1) (operandToString op2)
  | Neg op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)
  | Imul op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)  
  | Idiv op ->
    Printf.sprintf "\tidiv %s\n" (operandToString op)
  | Cmp (op1, op2) ->
    Printf.sprintf "\tcmp %s, %s\n" (operandToString op1) (operandToString op2)
  | Cwd ->
    Printf.sprintf "\tcwd\n"
  | And (op1, op2) ->
    Printf.sprintf "\tand %s, %s\n" (operandToString op1) (operandToString op2)
  | Or (op1, op2) ->
    Printf.sprintf "\tor %s, %s\n" (operandToString op1) (operandToString op2)
  | Xor (op1, op2) ->
    Printf.sprintf "\xor %s, %s\n" (operandToString op1) (operandToString op2)
  | Not op ->
    Printf.sprintf "\tnot %s\n" (operandToString op)  
  | Test op ->
    Printf.sprintf "\ttest %s\n" (operandToString op)  
  | Jmp (op1, op2) ->
    Printf.sprintf "\tjmp %s, %s\n" (operandToString op1) (operandToString op2)
  | CondJmp (str, op) ->
    Printf.sprintf "\t%s %s\n" s (operandToString op2)
  | Push op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)  
  | Pop op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)  
  | Call op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)  
  | Ret ->
    Printf.sprintf "\tret\n"
  | Fld op ->
    Printf.sprintf "\tneg %s\n" (operandToString op)  
  | Faddp (op1, op2) ->
    Printf.sprintf "\tsub %s, %s\n" (operandToString op1) (operandToString op2)
  | Fsubp (op1, op2) ->
    Printf.sprintf "\tsub %s, %s\n" (operandToString op1) (operandToString op2)
  | Fmulp (op1, op2) ->
    Printf.sprintf "\tsub %s, %s\n" (operandToString op1) (operandToString op2)
  | Fdivp (op1, op2) ->
    Printf.sprintf "\tsub %s, %s\n" (operandToString op1) (operandToString op2)
  | Fcompp -> 
    Printf.sprintf "\tfcompp\n"
  | Fstsw op ->
    Printf.sprintf "\tfstsw %s\n" (operandToString op)  
  | Fstp op -> 
    Printf.sprintf "\tfstp %s\n" (operandToString op)  
  | Fun str ->
    Printf.sprintf "%s proc near\n" str  
  | EndFun str ->
    Printf.sprintf "%s endp\n" str  
  | LabelDecl str ->
    Printf.sprintf "%s:\n" str

