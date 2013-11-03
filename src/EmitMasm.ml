open Final
open Types
open Error

let library_funs : (string * Types.typ * (string * Types.typ) list) list ref =
  ref []

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

let rec operandToString = function 
  | Reg r -> regToString r
  | Pointer (size, operand, i) when i = 0 -> 
    Printf.sprintf "%s ptr [%s]" (sizeToString size) (operandToString operand)
  | Pointer (size, operand, i) when i > 0 -> 
    Printf.sprintf "%s ptr [%s+%d]" (sizeToString size) 
      (operandToString operand) i
  | Pointer (size, operand, i) when i < 0 -> 
    Printf.sprintf "%s ptr [%s-%d]" (sizeToString size) 
      (operandToString operand) (-i)
  | Pointer _ -> 
    internal 
      "Because OCaml doesn't understand that the above matching is exhaustive"
  | LabelPtr (size, label) ->
    Printf.sprintf "%s ptr %s" 
      (sizeToString size) label
  | Label l -> l
  | Immediate i -> i


let constFloatToString flt id =
  let name = Final.fltLabel id in
    Printf.sprintf "\t%s\t tbyte %f\n" name flt

let constStringToString str id = 
  let size = String.length str in
  let rec fromText off s acc = 
    if (off = size) then acc ^ "\'"
    else
      match s.[off] with
        | '\x20' | '\x21' | '\x23' .. '\x26' | '\x28' .. '\x7E' ->
          fromText (off+1) s (acc ^ (Printf.sprintf "%c" s.[off])) 
        | _ ->
          fromAscii (off+1) s 
            (acc ^ (Printf.sprintf "\', %d" (Char.code s.[off]))) 
  and fromAscii off s acc =
    if (off = size) then acc
    else
      match s.[off] with
        | '\x20' | '\x21' | '\x23' .. '\x26' | '\x28' .. '\x7E' ->
          fromText (off+1) s (acc ^ (Printf.sprintf ", \'%c" s.[off])) 
        | _ ->
          fromAscii (off+1) s 
            (acc ^ (Printf.sprintf ", %d" (Char.code s.[off])))
  in
  let first off s =
    if (off = size) then "\'"
    else
      match s.[off] with
        | '\x20' | '\x21' | '\x23' .. '\x26' | '\x28' .. '\x7E' ->
          fromText (off+1) s (Printf.sprintf "\'%c" s.[off]) 
        | _ ->
          fromAscii (off+1) s (Printf.sprintf "%d" (Char.code s.[off]))
  in
    Printf.sprintf "\t%s\tword %d\n\t\t\tbyte %s, 0\n" (Final.strLabel id) 
      size (first 0 str)


let libFunDecl lst_lib lst_auxil = 
  let rec aux lst acc = 
    match lst with
      | [] -> acc
      | ("_dummy", _) :: tl -> aux tl acc
      | ("_delete", _) :: tl -> aux tl acc   (*until someone implements them*)
      | ("_delete_array", _) :: tl -> aux tl acc
      | (name, _) :: tl ->
        aux tl ((Printf.sprintf "\textrn\t %s : proc\n" ("_" ^ name)) ^ acc)
  in
  let rec lib lst acc =
    match lst with
      | [] -> "\n\n" ^ acc
      | (name, _, _) :: tl ->
        lib tl ((Printf.sprintf "\textrn\t %s : proc\n" ("_" ^ name)) ^ acc)
  in
  let auxiliary = aux lst_auxil "" in
    lib lst_lib auxiliary

let instructionToString = function
  | Prelude outer -> 
    ".model Small\n.stack 16384\n.data\n" ^
      (let floats = List.fold_left (fun acc (f, id) ->
           (constFloatToString f id) ^ acc) "" (!Final.flt_lst)  in
         (List.fold_left (fun acc (s, id) -> 
              (constStringToString s id) ^ acc) floats (!Final.str_lst))) ^ "\n"
    ^ ".code\n" ^ "main\tproc\tfar\n.Startup\n \
                   \t\tcall\tnear ptr " ^ (makeFunctionLabel outer) ^ "\n\
                                                                       \t\tmov\tax, 4C00h\n\
                                                                       \t\tint\t21h\n\
                                                                       main\tendp\n\
                                                                       .286\n"
  | Epilogue ->
    let externs = libFunDecl !library_funs Quads.auxil_funs in
      externs ^ "\t\tend"
  | Mov (op1, op2) ->
    Printf.sprintf "\t\tmov\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Lea (op1, op2) ->
    Printf.sprintf "\t\tlea\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Add (op1, op2) ->
    Printf.sprintf "\t\tadd\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Sub (op1, op2) ->
    Printf.sprintf "\t\tsub\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Inc op ->
    Printf.sprintf "\t\tinc\t%s\n" (operandToString op)
  | Dec op ->
    Printf.sprintf "\t\tdec\t%s\n" (operandToString op)
  | Neg op ->
    Printf.sprintf "\t\tneg\t%s\n" (operandToString op)
  | Imul op ->
    Printf.sprintf "\t\timul\t%s\n" (operandToString op)  
  | Idiv op ->
    Printf.sprintf "\t\tidiv\t%s\n" (operandToString op)
  | Cmp (op1, op2) ->
    Printf.sprintf "\t\tcmp\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Cwd ->
    Printf.sprintf "\t\tcwd\n"
  | And (op1, op2) ->
    Printf.sprintf "\t\tand\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Or (op1, op2) ->
    Printf.sprintf "\t\tor\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Xor (op1, op2) ->
    Printf.sprintf "\t\txor\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Not op ->
    Printf.sprintf "\t\tnot\t%s\n" (operandToString op)  
  | Test (op1, op2) ->
    Printf.sprintf "\t\ttest\t%s, %s\n" (operandToString op1) 
      (operandToString op2) 
  | Jmp op ->
    Printf.sprintf "\t\tjmp\t%s\n" (operandToString op)
  | CondJmp (str, op) ->
    Printf.sprintf "\t\t%s\t%s\n" str (operandToString op)
  | Push op ->
    Printf.sprintf "\t\tpush\t%s\n" (operandToString op)  
  | Pop op ->
    Printf.sprintf "\t\tpop\t%s\n" (operandToString op)  
  | Call op ->
    Printf.sprintf "\t\tcall\t%s\n" (operandToString op)  
  | Ret ->
    Printf.sprintf "\t\tret\n"
  | Fld op ->
    Printf.sprintf "\t\tfld\t%s\n" (operandToString op)  
  | Faddp (op1, op2) ->
    Printf.sprintf "\t\tfaddp\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Fsubp (op1, op2) ->
    Printf.sprintf "\t\tfsubp\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Fmulp (op1, op2) ->
    Printf.sprintf "\t\tfmulp\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Fdivp (op1, op2) ->
    Printf.sprintf "\t\tfdivp\t%s, %s\n" (operandToString op1) 
      (operandToString op2)
  | Fcompp -> 
    Printf.sprintf "\t\tfcompp\n"
  | Fstsw op ->
    Printf.sprintf "\t\tfstsw\t%s\n" (operandToString op)  
  | Fstp op -> 
    Printf.sprintf "\t\tfstp\t%s\n" (operandToString op)
  | Sahf ->
    Printf.sprintf "\t\tsahf\n"
  | Fun str ->
    Printf.sprintf "%s\tproc\tnear\n" str  
  | EndFun str ->
    Printf.sprintf "%s\tendp\n" str  
  | LabelDecl str ->
    Printf.sprintf "%s:\n" str
  | Comment str ->
    Printf.sprintf ";%s" str
  | Interrupt op ->
    Printf.sprintf "\t\tint\t%s\n" (operandToString op) 

let emit final lib_funs =
  let _ = library_funs := lib_funs in
    List.fold_left (fun asm instr -> asm ^ (instructionToString instr)) "" final
