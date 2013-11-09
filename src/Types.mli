exception PolymorphicTypes
type dim = D_Dim of int | D_DimSize of int | D_Alpha of int
type typ =
    T_Unit
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
  | T_Noarr
type string_const = { sval : string; spos : int * int; }
type char_const = { cval : string; cpos : int * int; }
type int_const = { ival : int; ipos : int * int; }
type float_const = { fval : float; fpos : int * int; }
type id_const = { id_name : string; id_pos : int * int; }
type cid_const = { cid_name : string; cid_pos : int * int; }
type bool_const = { bval : bool; bpos : int * int; }
type op = { pos : int * int; }
val tag_size : int
val ar_size : int
val sizeOfType : typ -> int
val sizeOfElement : typ -> int
val equalType : typ -> typ -> bool
val arrayDims : typ -> dim
val checkType : typ -> unit
