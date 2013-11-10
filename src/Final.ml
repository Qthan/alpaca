open Identifier
open Types
open SymbTypes
open Quads
open Symbol
open Error

(** available register types*)
type register = Ax | Bx | Cx | Dx | Di | Si | Bp | Sp
              | Al | Ah | Bl | Bh | Cl | Ch | Dl | Dh
              | ST of int

(** available size types *) 
type size = Byte | Word | TByte | DWord | Near

(** instruction operands *)
type operand =
  | Reg of register
  | Pointer of (size * operand * int)
  | LabelPtr of (size * string)
  | Label of string
  | Immediate of string

(**  available instruction types *)  
type instruction =
  | Prelude of entry
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

(** CPU word size*)
let word_size = 2

(** A reference to the current function compiled start with a dummy reference. *)
let current_fun = ref (Symbol.findAuxilEntry "_dummy")

(** An empty instruction list *)
let newInstrList () = []

(** A function adding a new instruction *)
let genInstr instr instr_lst = instr :: instr_lst 

(** A list holding strings and their unique id *)
let str_lst = ref []

let strLabel id = Printf.sprintf "@str%d" id

(** A closure for saving new strings *)
let saveString =
  let str_no = ref 0 in
    fun str -> incr str_no; 
      str_lst := (str, !str_no) :: !str_lst;
      strLabel !str_no

(** A list holding float constants and their unique id*)
let flt_lst = ref []

let fltLabel id = Printf.sprintf "@flt%d" id
(** A closure for saving new floats *)
let saveFloat =
  let flt_no = ref 0 in
    fun flt -> incr flt_no;
      flt_lst := (flt, !flt_no) :: !flt_lst;
      fltLabel !flt_no

(* A function returning the nesting level of an entry *)
let rec getNesting entry =
  match entry.entry_info with
    | ENTRY_function f -> f.function_nesting
    | ENTRY_variable v -> v.variable_nesting
    | ENTRY_parameter p -> p.parameter_nesting
    | ENTRY_temporary _ -> getNesting !current_fun
    | _ -> internal "Nesting not applicable at not function/variable/parameter"

(* A function returning the size of a type *)
let getTypeSize typ =
  match typ with
    | T_Int | T_Ref _ 
    | T_Array (_, _) | T_Id _ -> Word
    | T_Char | T_Bool -> Byte
    | T_Arrow _ -> DWord
    | T_Float -> TByte
    | _ -> internal "Wrong type in getSize"

(* A function returning the size of an entry's type *)
let getSize e =
  let typ = getType e in
    getTypeSize typ

(* A function returning the size of referenced object *)
let getRefSize e =
  match getType e with
    | T_Ref typ -> getTypeSize typ
    | _ -> internal "Deref on a non-ref type?"

(* A function returning the size in bytes *)
let sizeToBytes = function
  | Byte -> "1"
  | Word -> "2"
  | TByte -> "10"
  | DWord -> "4"
  | Near -> internal "not a size type"

(* A function returning a suitable register according to the type size *)
let getRegister r ty =
  match getTypeSize ty with
    | Word -> r
    | Byte -> 
      ( match r with
        | Ax -> Al
        | Bx -> Bl
        | Cx -> Cl
        | Dx -> Dl
        | Di | Si | Bp | Sp | ST _ -> internal "unused registers"
        | Al | Ah | Bl | Bh | Cl | Ch| Dl| Dh -> 
          internal "unapplicable registers")
    | TByte -> internal "not applicable"
    | DWord -> internal "unused"
    | Near -> internal "not a size"

(* A function returning the label of a function*) 
let makeFunctionLabel e = 
  match e.entry_info with
    | ENTRY_function f  when f.function_library = false -> 
      "_p_" ^ (id_name e.entry_id) ^ "_" ^ (string_of_int f.function_index)
    | ENTRY_function f -> "_" ^ (id_name e.entry_id)
    | ENTRY_parameter _ | ENTRY_variable _ -> 
      internal "Cannot call this like that"
    | _ -> internal "cannot call non function"

(* A function returning the label of a quad*)     
let makeLabel n = 
  match n with 
    | O_Label n -> "@" ^ (string_of_int n) 
    | _ -> internal "expecting label"

(* A function returning the jump instruction *)
let relOpJmp = function 
  | Q_L -> "jl"
  | Q_Le -> "jle" 
  | Q_G -> "jg"
  | Q_Ge -> "jge"
  | Q_Seq | Q_Eq -> "je"
  | Q_Nseq | Q_Neq -> "jne"
  | _ -> internal "Not a relative operator"

let revertCond = function
  | "jl" -> "jge"
  | "jle" -> "jg"
  | "jg" -> "jle"
  | "jge" -> "jl"
  | "je" -> "jne"
  | "jne" -> "je"
  | "jb" -> "jae"
  | "jbe" -> "ja"
  | "ja" -> "jbe"
  | "jae" -> "jb"
  | "jnz" -> "jz"
  | str -> internal "unsupported relop %s\n" str


(* http://stackoverflow.com/questions/7057501/
 *  x86-assembler-floating-point-compare *)
let relOpJmpF = function
  | Q_G -> "jb"
  | Q_Ge -> "jbe"
  | Q_L -> "ja"
  | Q_Le -> "jae"
  | Q_Seq | Q_Eq -> "je"
  | Q_Nseq | Q_Neq -> "jne"
  | _ -> internal "unconverted relop or bug"

(* A function returning the result type of a function *)
let functionResult e = 
  let typ = getResType e in
  let rec dearrow = function
    | T_Arrow (_, t) -> dearrow t
    | t -> t
  in dearrow typ

(* A function returning the activation record in which a lives *)
let getAR a instr_lst =
  let a_nest = getNesting a in
  let f_nest = getNesting (!current_fun) in
  let rec aux i acc =
    match i with
      | 0 -> acc
      | i -> 
        aux (i-1) 
          (genInstr (Mov (Reg Si, Pointer (Word, Reg Si, 2*word_size))) acc)
  in
    aux (f_nest - a_nest -1) 
      (genInstr (Mov (Reg Si, Pointer (Word, Reg Bp, 2*word_size))) instr_lst)


(* A function for loading a into register r *)
let rec load r a instr_lst =
  match a with
    | O_Int i -> genInstr (Mov (Reg r, Immediate (string_of_int i))) instr_lst
    | O_Bool b when b = true -> genInstr (Mov (Reg r, Immediate "1")) instr_lst
    | O_Bool b -> genInstr (Mov (Reg r, Immediate "0")) instr_lst
    | O_Char c -> 
      genInstr (Mov (Reg r, Immediate (string_of_int (Char.code c.[0])))) 
        instr_lst
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
          | n -> internal "too internal to judge")
    | O_Deref op ->
      let size = 
        match op with 
          | O_Entry e -> getRefSize e 
          | _ -> internal "Cannot load Deref: requires O_Entry" 
      in
      let instr_lst1 = load Di op instr_lst in
        genInstr (Mov (Reg r, Pointer (size, Reg Di, 0))) instr_lst1
    | O_Str _ -> loadAddress r a instr_lst
    | _ -> internal "Cannot load: unmatched operand"

(* A function for loading the address of a into register r*)
and loadAddress r a instr_lst =
  match a with 
    | O_Str str -> 
      let operand = saveString str in
        genInstr (Lea (Reg r, LabelPtr (Byte, operand))) instr_lst
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
          | n -> internal "too internal to judge")
    | O_Deref op -> load r op instr_lst
    | _ -> internal "Must be a string or potentialy an entry"

(* A function for loading float values into float stack *)
let loadReal a instr_lst =
  match a with
    | O_Float f -> 
      let operand = saveFloat f in
        genInstr (Fld (Label operand)) instr_lst
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
          | n  -> internal "too internal to judge")
    | O_Deref op -> 
      let size = 
        match op with 
          | O_Entry e -> getRefSize e 
          | _ -> internal "Cannot load Deref: requires O_Entry" 
      in
      let instr_lst1 = load Di op instr_lst in
        genInstr (Fld (Pointer (size, Reg Di, 0))) instr_lst1
    | _ -> internal "must be a float or an entry"

(* A function for storing register's r contents into a *)
let store r a instr_lst =
  match a with
    | O_Entry e ->
      let c_nest = getNesting (!current_fun) in
      let e_nest = getNesting e in
      let size = getSize e in
      let offset = getOffset e in
        (match c_nest - e_nest with
          | 0 ->
            genInstr (Mov (Pointer (size, Reg Bp, offset), Reg r)) instr_lst
          | n when n > 0 ->
            let instr_lst1 = getAR e instr_lst in
              genInstr (Mov (Pointer (size, Reg Si, offset), Reg r)) instr_lst1
          | _ -> internal "Can't do that")
    | O_Deref op ->
      let size =
        match op with
          | O_Entry e -> getRefSize e
          | _ -> internal "Must be Entry"
      in
      let instr_lst1 = load Di op instr_lst in
        genInstr (Mov (Pointer (size, Reg Di, 0), Reg r)) instr_lst1
    | O_Res ->
      let size = getTypeSize (getResType !current_fun) in
      let instr_lst1 = 
        genInstr (Mov (Reg Si, Pointer (Word, Reg Bp, 3*word_size))) instr_lst 
      in
      let instr_lst2 = 
        genInstr (Mov (Pointer (size, Reg Si, 0), Reg r)) instr_lst1 
      in
        instr_lst2
    | _ -> internal "must be an entry"

(* A function for storing float stack contents into a *)
let storeReal a instr_lst =
  match a with
    | O_Entry e ->
      let c_nest = getNesting (!current_fun) in
      let e_nest = getNesting e in
      let size = getSize e in
      let offset = getOffset e in
        (match c_nest - e_nest with
          | 0 ->
            genInstr (Fstp (Pointer (size, Reg Bp, offset))) instr_lst
          | n when n > 0 ->
            let instr_lst1 = getAR e instr_lst in
              genInstr (Fstp (Pointer (size, Reg Si, offset))) instr_lst1
          | _ -> internal "Can't do that")
    | O_Deref op ->
      let size =
        match op with
          | O_Entry e -> getRefSize e
          | _ -> internal "Must be entry"
      in
      let instr_lst1 = load Di op instr_lst in
        genInstr (Fstp (Pointer (size, Reg Di, 0))) instr_lst1
    | O_Res ->
      let size = getTypeSize (getResType !current_fun) in
      let instr_lst1 = 
        genInstr (Mov (Reg Si, Pointer (Word, Reg Bp, 3*word_size))) instr_lst 
      in
      let instr_lst2 = genInstr (Fstp (Pointer (size, Reg Si, 0))) instr_lst1 in
        instr_lst2
    | _ -> internal "must be a float or an entry"

(* A function for loading a function's enviroment into register r *)
let loadFunEnv r e instr_lst =
  match e with 
    | O_Entry e ->
      let offset = match e.entry_info with
        | ENTRY_function f -> 2*word_size
        | _ -> Symbol.getOffset e   
      in    
      let c_nest = getNesting (!current_fun) in
      let f_nest = getNesting e in
      let instr_lst1 = match c_nest = f_nest with
        | true -> 
          genInstr (Mov (Reg r, (Pointer (Word, Reg Bp, offset)))) instr_lst
        (* if parameter or variable get here semantic check has failed*)
        | false when c_nest < f_nest -> genInstr (Mov (Reg r, Reg Bp)) instr_lst 
        | false ->  
          let instr_lst2 = getAR e instr_lst in
            genInstr (Mov (Reg r, (Pointer (Word, Reg Si, offset))))  instr_lst2
      in
        instr_lst1
    | O_Deref op ->
      let instr_lst1 = load Di op instr_lst in
      let instr_lst2 = 
        genInstr (Mov (Reg r, Pointer (Word, Reg Di, 0))) instr_lst1 
      in
        instr_lst2
    | _ -> internal "Only entry or deref can be fun"

(* A function for loading a function's code into register r *)
let loadFunCode r e instr_lst =
  match e with 
    |  O_Entry e ->
      (match e.entry_info with
        | ENTRY_function f ->
          let code_ptr = makeFunctionLabel e in
          let instr_lst1 = 
            genInstr (Lea (Reg r, LabelPtr (Near, code_ptr))) instr_lst 
          in
            instr_lst1
        | ENTRY_variable _ | ENTRY_parameter _ | ENTRY_temporary _ ->
          let offset = getOffset e in
          let c_nest = getNesting (!current_fun) in
          let f_nest = getNesting e in
          let instr_lst1 = match c_nest = f_nest with
            | true -> 
              let off = offset + word_size in
                genInstr (Mov (Reg r, Pointer (Word, Reg Bp, off))) 
                  instr_lst
            | false when c_nest < f_nest -> 
              internal "trying to call unreachable variable"
            | false ->
              let instr_lst2 = getAR e instr_lst in
              let off = offset + word_size in
                genInstr (Mov (Reg r, Pointer (Word, Reg Si, off))) instr_lst2
          in
            instr_lst1
        | ENTRY_udt _ | ENTRY_none
        | ENTRY_constructor _ -> internal "Cannot have temp/udt/cons here")
    | O_Deref op ->
      let instr_lst1 = load Di op instr_lst in
      let instr_lst2 = 
        genInstr (Mov (Reg r, Pointer (Word, Reg Di, word_size))) instr_lst1 
      in
        instr_lst2
    | _ -> internal "Only entry or deref can be fun"    

(* A function for loading a function's code into register r1 
 * and enviroment into register r2 *)
let loadFun r1 r2 e instr_lst =
  let instr_lst1 = loadFunCode r1 e instr_lst in
  let instr_lst2 = loadFunEnv r2 e instr_lst1 in
    instr_lst2

(* A function for updating the active link of the callee *)    
let updateAL callee_entry instr_lst =
  let instr_lst1 = loadFunEnv Si callee_entry instr_lst in
    genInstr (Push (Reg Si)) instr_lst1

(* A function for storing a function's code from register r1 
 * and enviroment from register r2 into a*)
let storeFun r1 r2 a instr_lst = 
  match a with
    | O_Entry e ->
      let c_nest = getNesting (!current_fun) in
      let e_nest = getNesting e in
      let offset = getOffset e in
        (match c_nest - e_nest with
          | 0 ->
            let instr_lst1 = 
              genInstr (Mov (Pointer (Word, Reg Bp, offset+word_size), Reg r1)) 
                instr_lst 
            in
            let instr_lst2 = 
              genInstr (Mov (Pointer (Word, Reg Bp, offset), Reg r2)) 
                instr_lst1
            in
              instr_lst2
          | n when n > 0 -> (* this must be redundant *)
            let instr_lst1 = getAR e instr_lst in
            let instr_lst2 = 
              genInstr (Mov (Pointer (Word, Reg Si, offset+word_size), Reg r1)) 
                instr_lst1
            in
            let instr_lst3 = 
              genInstr (Mov (Pointer (Word, Reg Si, offset), Reg r2)) instr_lst2 
            in
              instr_lst3
          | _ -> internal "Can't do that")
    | O_Deref op ->
      let instr_lst1 = load Di op instr_lst in
      let instr_lst2 = 
        genInstr (Mov (Pointer (Word, Reg Di, 0), Reg r1)) instr_lst1
      in
      let instr_lst3 =
        genInstr (Mov (Pointer (Word, Reg Di, word_size), Reg r2)) instr_lst2 
      in
        instr_lst3
    | _ -> internal "must be an entry" 
(*
let printAR f =
  let rec var_info lst = 
    match lst with
      | [] -> []
      | x :: xs -> (x.entry_id, getOffset x, getType x) :: (var_info xs)
  in
  let print_list = List.iter (fun (id, offset, typ) -> 
 Format.printf "%d: %a : %a\n" offset pretty_id id pretty_typ typ) in
  match f.entry_info with 
    | ENTRY_function f ->

       print_list (var_info f.function_paramlist);
       Printf.printf "----------------\n";
       print_list (var_info f.function_varlist);
    | _ -> internal "i don't want to do that"  
 *)

