open Identifier
open Error

exception PolymorphicTypes
(** Llama Types **)

type dim =
  | D_Int of int 
  | D_Alpha of int

type typ = 
  | T_Unit 
  | T_Int
  | T_Char
  | T_Str (* T_Str is wrong, use array of chars instead*)
  | T_Bool
  | T_Float
  | T_Notype
  | T_Ord
  | T_Arrow of typ * typ
  | T_Ref of typ
  | T_Array of typ * dim
  | T_Id of string
  | T_Alpha of int


(** Parser Types **)

type string_const = { 
  sval : string; 
  spos : (int * int)
}

type char_const = { 
  cval : string; 
  cpos : (int * int)
}

type int_const = {
  ival : int; 
  ipos : (int * int)
}

type float_const = { 
  fval : float; 
  fpos : (int * int)
}

type id_const = { 
  id_name : string; 
  id_pos : (int * int)
}

type cid_const = { 
  cid_name : string; 
  cid_pos : (int * int)
}

type bool_const = {
  bval : bool; 
  bpos : (int * int)
}

type op = {
  pos : (int * int)
}




let rec sizeOfType t =
  match t with
    | T_Int            -> 2
    | T_Float          -> 10
    (*| TYPE_byte           -> 1*)
    | T_Array (et, sz) -> (sizeOfType et)
    | T_Char           -> 1
    | T_Bool           -> 1
    | _                -> 0

let rec equalType t1 t2 =
  match t1, t2 with
    | T_Array (et1, sz1), T_Array (et2, sz2) -> equalType et1 et2
    | _ -> t1 = t2

let arrayDims a =
  match a with
    | T_Array (_, dims) -> dims
    | _ -> failwith "must be an array\n"

(* is this working? *)
let rec checkType typ =
  match typ with
    | T_Alpha _ | T_Ord -> raise PolymorphicTypes
    | T_Array (_, D_Alpha _) -> raise PolymorphicTypes
    | T_Array (t, _) -> checkType t 
    | T_Ref t -> checkType t
    | T_Notype -> internal "Invalid type \n"
    | T_Arrow _ -> internal "I used to be a valid type but then I took an arrow to the knee \n"
    | _ -> ()
