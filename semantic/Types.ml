open Identifier

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

(** Symbol Types **)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE
  | PARDEF_DEFINE
  | PARDEF_CHECK

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool
}

and variable_info = {
  mutable variable_type   : typ;
  variable_offset         : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result    : typ;
  mutable function_pstatus   : param_status;
  mutable function_initquad  : int
}

and parameter_info = {
  mutable parameter_type   : typ;
  mutable parameter_offset : int;
  parameter_mode           : pass_mode
}

and temporary_info = {
  temporary_type   : typ;
  temporary_offset : int
}

and constructor_info = {
  constructor_type    : typ;
  constructor_paramlist : typ list
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info
               | ENTRY_udt 
               | ENTRY_constructor of constructor_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

(** Ast Types **)

type binop =
     | Plus | Fplus | Minus| Fminus
     | Times| Ftimes| Div  | Fdiv 
     | Mod  | Power | Seq  | Nseq
     | L    | Le    | G    | Ge
     | Eq   | Neq
     | And  | Or
     | Semicolon | Assign

type unop =
    | U_Plus | U_Fplus
    | U_Minus| U_Fminus
    | U_Del
    | U_Not

type sign =
    | P_Plus
    | P_Minus

type fsign =
    | P_Fplus
    | P_Fminus

type count =
    | To
    | Downto

type intmb =
    | Yesnum of int
    | Nonum

and ast_atom_node = {
  atom                   : ast_atom;
  atom_pos               : (int * int);
  mutable atom_typ       : typ;
  mutable atom_entry     : entry option
}

and ast_atom =
    | A_Num of int
    | A_Dec of float
    | A_Chr of string
    | A_Str of string
    | A_Bool of bool
    | A_Cid of string
    | A_Var of string
    | A_Par 
    | A_Bank of ast_atom_node
    | A_Array of string * ast_expr_node list
    | A_Expr of ast_expr_node

and ast_expr_node = {
  expr                   : ast_expr;
  expr_pos               : (int * int);
  mutable expr_typ       : typ;
  mutable expr_entry     : entry option
}

and ast_expr =
    | E_Binop of ast_expr_node * binop * ast_expr_node
    | E_Unop of unop * ast_expr_node
    | E_Block of ast_expr_node
    | E_While of ast_expr_node * ast_expr_node
    | E_For of string * ast_expr_node * count * ast_expr_node * ast_expr_node
    | E_Atom of ast_atom_node
    | E_Dim of intmb * string
    | E_Ifthenelse of ast_expr_node * ast_expr_node * ast_expr_node
    | E_Ifthen of ast_expr_node * ast_expr_node
    | E_Id of string * ast_atom_node list
    | E_Cid of string * ast_atom_node list
    | E_Match of ast_expr_node * ast_clause list
    | E_Letin of ast_stmt * ast_expr_node
    | E_New of typ

and ast_clause =
      Clause of ast_pattern_node * ast_expr_node

and ast_pattern_node = {
  pattern                     : ast_pattern;
  pattern_pos                 : (int * int);
  mutable pattern_typ         : typ;
  mutable pattern_entry       : entry option
}

and ast_pattern =
    | Pa_Atom of ast_pattom_node
    | Pa_Cid of string * ast_pattom_node list

and ast_pattom_node = {
  pattom                    : ast_pattom;
  pattom_pos                : (int * int);
  mutable pattom_typ        : typ;
  mutable pattom_entry      : entry option
}

and ast_pattom =
    | P_Sign of sign * int
    | P_Fsign of fsign * float
    | P_Num of int
    | P_Float of float
    | P_Chr of string
    | P_Bool of bool
    | P_Id of string
    | P_Cid of string
    | P_Pattern of ast_pattern_node

and ast_def_node = {
  def                  : ast_def;
  def_pos              : (int * int);
  mutable def_entry    : entry option
}

and ast_def =
    | D_Var of (string * typ) list * ast_expr_node
    | D_Mut of (string * typ) 
    | D_Array of string * typ * ast_expr_node list

and ast_stmt =
    | S_Let of ast_def_node list
    | S_Rec of ast_def_node list
    | S_Type of  (string * ( ( string*typ list ) list) ) list
    
(* Intermediate Types *)


type quad_operators =
  | Q_Unit | Q_Endu
  | Q_Plus | Q_Minus | Q_Mult | Q_Div | Q_Mod
  | Q_Fplus | Q_Fminus | Q_Fmult | Q_Fdiv | Q_Pow
  | Q_L | Q_Le | Q_G | Q_Ge | Q_Seq | Q_Nseq
  | Q_Eq | Q_Neq (* Physical equality *)
  | Q_Assign | Q_Ifb | Q_Array
  | Q_Jump | Q_Jumpl | Q_Label
  | Q_Call | Q_Par | Q_Ret

type quad_operands = 
  | O_Int of int
  | O_Char of char
  | O_Bool of bool
  | O_Backpatch
  | O_Label of int
  | O_Temp of int * typ
  | O_Res (* $$ *)
  | O_Ret (* RET *)
  | O_ByVal
  | O_Fun of string
  | O_Obj of string
  | O_Empty
  | O_Ref of quad_operands
  | O_Deref of quad_operands
  | O_Size of int
  | O_Dims of int

type quad = {
  label : int;
  operator : quad_operators;
  arg1 : quad_operands;
  arg2 : quad_operands;
  mutable arg3 : quad_operands
}

type expr_info = {
  place : quad_operators option;
  next  : int list
}

type cond_info = {
  true_lst  : int list;
  false_lst : int list
}

type stmt_info = { 
    next : int list
}

let rec sizeOfType t =
   match t with
     | T_Int            -> 2
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
(*
let rec checkType typ =
  match typ with
    | T_Alpha _ | T_Ord -> raise PolymorphicTypes
    | T_Array (_, D_Alpha _) -> raise PolymorphicTypes
    | T_Array (t, _) -> checkType t 
    | T_Ref t -> checkType t
    | T_Notype -> internal "Invalid type \n"
    | T_Arrow _ -> internal "I used to be a valid type but then I took an arrow to the knee \n"
    | _ -> () *)
