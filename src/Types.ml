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
  | T_Bool
  | T_Float
  | T_Arrow of typ * typ
  | T_Ref of typ
  | T_Array of typ * dim
  | T_Id of string
  | T_Alpha of int
  | T_Notype
  | T_Ord
  | T_Nofun


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


let tag_size = 2 (* Tag of UDT constructors *)
let ar_size = 8 (* Size of the static part of the AR *)

let rec sizeOfType t =
  match t with
    | T_Int            -> 2
    | T_Float          -> 10
    | T_Array (et, sz) -> 2
    | T_Char           -> 1
    | T_Bool           -> 1
    | T_Unit           -> 0
    | T_Ref typ        -> 2
    (* 2 bytes for code pointer and 2 bytes for enviroment pointer *)
    | T_Arrow (_, _)   -> 4  
    | T_Id _           -> 2
    | T_Alpha _ | T_Notype 
    | T_Ord | T_Nofun -> internal "Cannot resolve size for these types"

let rec sizeOfElement t =
  match t with
    | T_Array (typ, sz) -> sizeOfElement typ
    | T_Ref typ        -> sizeOfElement typ
    | _ -> sizeOfType t                           

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
    | T_Alpha _ | T_Ord | T_Nofun -> raise PolymorphicTypes
    | T_Array (_, D_Alpha _) -> raise PolymorphicTypes
    | T_Array (t, _) -> checkType t 
    | T_Ref t -> checkType t
    | T_Notype -> internal "Invalid type \n"
    | T_Arrow (t1, t2) -> checkType t1; checkType t2 
    | _ -> ()
