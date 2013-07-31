open Symbtypes

type register = Ax | Bx | Cx | Dx | Di | Si | Bp | Sp
              | Al | Ah | Bl | Bh | Cl | Ch | Dl | Dh
              
type operand = Reg of register | Immediate of int

type size = Byte | Word 

type address =
  | Reg of register
  | Pointer of (size * register * int)
  | Label of string
  | Immediate of string
  
type instruction =
  | Prelude
  | Epilogue
  | Mov of address * address
  | Lea of address * address
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
  | Jmp of address
  | CondJmp of string * address
  | Push of address
  | Pop of operand
  | Call of address
  | Ret
  
let word_size = 2

let current_fun = ref dummy

let newInstrList () = []

let genInstr instr instr_lst = instr :: instr_lst 

let getNesting entry =
  match entry.entry_info with
    | ENTRY_function f -> f.function_nesting
    | ENTRY_variable v -> v.variable_nesting
    | ENTRY_parameter p -> p.parameter_nesting
    | _ -> internal "Nesting not applicable at not function/variable/parameter"

let getAR a instr_lst =
  let a_nest = getNesting a in
  let f_nest = getNesting (!current_fun) in
  let rec aux i acc =
    match i with
      | 0 -> acc
      | i -> aux (i-1) (genInstr (Mov (Reg Si, Pointer (Word, Reg Si, 2*word_size))) acc)
  in
    aux (f_nest - a_nest -1) (genInstr (Mov (Reg Bp, Pointer (Word, Reg Bp, 2*word_size))) instr_lst)
    
let updateAL callee_entry instr_lst =
  match callee_entry.entry_info with
    | ENTRY_function f ->
        let p_nest = getNesting (!current_fun) in
        let x_nest = f.function_nesting in
          (match p_nest = x_nest with
            | true -> genInstr (Push (Pointer (Word, Reg Bp, 2*word_size))) instr_lst
            | false when p_nest < x_nest -> genInstr (Push (Reg Bp)) instr_lst
            | false -> 
                let rec aux i acc =
                  match i with
                    | 0 -> genInstr (Push (Pointer (Word,Reg Si, 2*word_size))) acc
                    | i -> aux (i-1) (genInstr (Mov (Reg Si, Pointer (Word,Reg Si, 2*word_size))) acc) 
                in
                  aux (p_nest - x_nest - 1) (genInstr (Mov (Reg Si, Pointer (Word,Reg Bp, 2*word_size))) instr_lst))
    | ENTRY_parameter p ->  
        let offset = p.parameter_offset in
        let instr_lst1 = genInstr (Mov (Reg Si, Pointer (Word, Reg Bp, offset + word_size))) instr_lst in
        let instr_lst2 = genInstr (Push (Reg Si)) instr_lst1 in
          instr_lst2
    | ENTRY_variable v -> 
        let offset = v.variable_offset in
        let instr_lst1 = genInstr (Mov (Reg Si, Pointer (Word, Reg Bp, offset + word_size))) instr_lst in
        let instr_lst2 = genInstr (Push (Reg Si)) instr_lst1 in
          instr_lst2

let load r a instr_lst =
  match a with
    | O_Int i -> genInstr (Mov (Reg r, Immediate (string_of_int i))) instr_lst
    | O_Bool b when b = true -> genInstr (Mov (Reg r, Immediate "1")) instr_lst
    | O_Bool b -> genInstr (Mov (Reg r, Immediate "0")) instr_lst
    | O_Char c -> genInstr (Mov (Reg r, Immediate (Char.code c.[0]))) instr_lst
    | O_Entry e -> 
        let c_nest = getNesting (!current_fun) in
        let e_nest = getNesting e in
          match c_nest - e_nest with
            | 0 -> 
                genInstr (Mov (Reg r, s


let rec sizeOfType t =
  match t with
    | T_Int            -> 2
    | T_Float          -> 10
    | T_Array (et, sz) -> (sizeOfType et)
    | T_Char           -> 1
    | T_Bool           -> 1
    | T_Unit           -> 0
    | T_Ref typ        -> 2
    | T_Arrow (_, _)   -> 4  (* 2 bytes for code pointer and 2 bytes for enviroment pointer *)
    | T_Id _           -> 0
    | T_Alpha _ | T_Notype 
    | T_Ord | T_Nofun -> internal "Cannot resolve size for these types"
let rec getSize e =
  let typ = getType e in
    match typ with
      | T_Int | T_Ref _ | T_Array (_, _) -> Word
      | T_Char | T_Bool -> Byte
      | T_Arrow -> internal "Cannot load function"
(*
f : 1
add : 2
app : 2
y : 2
f : 2

let f x = 
  let add y = x + y in
  let app f = f 3 in
    app add

a : 1
f : 1
add : 1
x : 1
app : 2
g : 2

let a = 3

let add y = a + y

let f a = 
  let app g = g a in
   app add


let addXY x =
    let addY y = 
        let addz z = x+y+z in
        addz 5 in
    addY x*)
