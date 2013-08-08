open Symbtypes
open Quads
open Symbol
open Error

type register = Ax | Bx | Cx | Dx | Di | Si | Bp | Sp
              | Al | Ah | Bl | Bh | Cl | Ch | Dl | Dh
              | ST of int
              
type size = Byte | Word | TByte

type operand =
  | Reg of register
  | Pointer of (size * register * int)
  | LabelPtr of (size * string)
  | Label of string
  | Immediate of string
  
type instruction =
  | Prelude
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
  | Fdiv of operand * operand
  | Fcompp
  | Fstsw operand
  | Fun of string
  
let word_size = 2

let current_fun = ref dummy

let newInstrList () = []

let genInstr instr instr_lst = instr :: instr_lst 

let str_lst = ref []

let saveString =
  let str_no = ref 0 in
    fun str -> incr str_no; 
      str_lst := (str, str_no) :: !str_lst;
      Printf.sprintf "@str%d" str_no

let rec getNesting entry =
  match entry.entry_info with
    | ENTRY_function f -> f.function_nesting
    | ENTRY_variable v -> v.variable_nesting
    | ENTRY_parameter p -> p.parameter_nesting
    | ENTRY_temporary _ -> getNesting !current_fun
    | _ -> internal "Nesting not applicable at not function/variable/parameter"

let getSize e =
  let typ = getType e in
    getTypeSize typ

let getTypeSize typ =
  match typ with
    | T_Int | T_Ref _ | T_Array (_, _) -> Word
    | T_Char | T_Bool -> Byte
    | T_Arrow _ -> internal "Cannot load function"
    | T_Float -> TByte
    | _ -> internal "Wrong type in getSize"


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

let rec load r a instr_lst =
  match a with
    | O_Int i -> genInstr (Mov (Reg r, Immediate (string_of_int i))) instr_lst
    | O_Bool b when b = true -> genInstr (Mov (Reg r, Immediate "1")) instr_lst
    | O_Bool b -> genInstr (Mov (Reg r, Immediate "0")) instr_lst
    | O_Char c -> genInstr (Mov (Reg r, Immediate (Char.code c.[0]))) instr_lst
    | O_Entry e -> 
        let c_nest = getNesting (!current_fun) in
        let e_nest = getNesting e in
          (match c_nest - e_nest with
            | 0 -> 
        let size = getSize e in
	      let offset = getOffset e in
		genInstr (Mov (Reg r, Pointer (size, Reg Bp, offset))) instr_lst  
	    | n when n > 0 ->
	      let instr_lst1 = getAR e instr_lst in
	      let size = getSize e in
	      let offset = getOffset e in
	      genInstr (Mov (Reg r, Pointer (size, Reg Si, offset))) instr_lst1
	    | n when n < 0 -> internal "too internal to judge")
    | O_Deref op ->
      let size = 
	match op with 
	  | O_Entry e -> getSize e 
	  | _ -> internal "Cannot load Deref: requires O_Entry" 
      in
      let instr_lst1 = load (Di, op) instr_lst in
	genInstr (Mov (Reg r, Pointer (size, Reg Di, 0))) instr_lst1
    | O_Str _ -> internal "Cannot load string"
    | _ -> internal "Cannot load: unmatched operand"

let loadAddress r a instr_list =
  match a with 
    | O_Str str -> 
      let operand = saveString str in
	genInstr (Lea (Reg r, LabelPtr (Byte, operand))) instr_list
    | O_Entry e ->
      let c_nest = getNesting (!current_fun) in
      let e_nest = getNesting e in
	(match c_nest - e_nest with
          | 0 -> 
	    let size = getSize e in
	    let offset = getOffset e in
	      genInstr (Lea (Reg r, Pointer (size, Reg Bp, offset))) instr_lst  
	  | n when n > 0 ->
	    let instr_lst1 = getAR e instr_lst in
	    let size = getSize e in
	    let offset = getOffset e in
	      genInstr (Lea (Reg r, Pointer (size, Reg Si, offset))) instr_lst1
	  | n when n < 0 -> internal "too internal to judge")
    | O_Deref op -> load r op instr_lst

let loadReal a instr_lst =
  match a with
    | O_Float f -> 
      genInstr (Fld string_of_float f) instr_lst
    | O_Entry e ->
      let c_nest = getNesting (!current_fun) in
      let e_nest = getNesting e in
        (match c_nest - e_nest with
          | 0 -> 
              let size = getSize e in
              let offset = getOffset e in
	      genInstr (Fld (Pointer (size, Reg Bp, offset))) instr_lst  
	  | n when n > 0 ->
	    let instr_lst1 = getAR e instr_lst in
	    let size = getSize e in
	    let offset = getOffset e in
	      genInstr (Fld (Pointer (size, Reg Si, offset))) instr_lst1
	  | n when n < 0 -> internal "too internal to judge")
    | O_Deref op -> 
      let size = 
        match op with 
          | O_Entry e -> getSize e 
          | _ -> internal "Cannot load Deref: requires O_Entry" 
      in
      let instr_lst1 = load Di op instr_lst in
        genInstr (Fld (Pointer (size, Reg Di, 0))) instr_lst1

let store r a instr_lst =
  match a with
    | O_Entry e ->
        let c_nest = getNesting (!current_fun) in
        let e_nest = getNesting e in
          (match c_nest - e_nest with
            | 0 ->
                let size = getSize e in
                let offset = getOffset e in
                  genInstr (Mov (Pointer (size, Reg Bp, offset), Reg r)) instr_lst
            | n when n > 0 ->
                let instr_lst1 = getAR e instr_lst in
                  genInstr (Mov (Pointer (size, Reg Si, offset), Reg r)) instr_lst1
            | _ -> internal "Can't do that")
    | O_Deref op ->
        let size =
          match op with
            | O_Entry e -> getSize e
            | _ -> internal "Must be Entry"
        in
        let instr_lst1 = load Di op instr_lst in
          genInstr (Move (Pointer (size, Reg Di, 0), Reg r)) instr_lst1

let storeReal a instr_lst =
  match a with
    | O_Entry e ->
        let c_nest = getNesting (!current_fun) in
        let e_nest = getNesting e in
          (match c_nest - e_nest with
             | 0 ->
                let size = getSize e in
                let offset = getOffset e in
                  genInstr (Fld (Pointer (size, Reg Bp, offset))) instr_lst
            | n when n > 0 ->
                let instr_lst1 = getAR e instr_lst in
                  genInstr (Fld (Pointer (size, Reg Si, offset))) instr_lst1
            | _ -> internal "Can't do that")
    | O_Deref op ->
        let size =
          match op with
            | O_Entry e -> getSize e
            | _ -> internal "Must be entry"
        in
        let instr_lst1 = load Di op instr_lst in
          genInstr (Fld (Pointer (size, Reg Di, 0))) instr_lst1
        
let makeFunctionLabel e = 
  match e.entry_info with
    | ENTRY_function f -> "_p_" ^ (id_name e.id) ^ "_" ^ (int_of_string f.function_index)
    | ENTRY_parameter _ | ENTRY_variable _ -> internal "Cannot call this like that"
    | _ -> internal "cannot call non function"
    
let makeLabel n = 
  match n with 
  | O_Label n -> "@" ^ (string_of_int n) 
  | _ -> internal "expecting label"
  
let relOpJmp = function 
  | Q_L -> "jl"
  | Q_Le -> "jle" 
  | Q_G -> "jg"
  | Q_Ge -> "jge"
  | Q_Seq -> "je"
  | Q_Nseq -> "jne"
  | _ -> internal "Not a relative operator"

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

