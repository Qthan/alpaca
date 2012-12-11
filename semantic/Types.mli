type typ =
    T_Unit
  | T_Int
  | T_Char
  | T_Str
  | T_Bool
  | T_Float
  | T_Notype
  | T_Arrow of typ * typ
  | T_Ref of typ
  | T_Array of typ * int
  | T_Id of string
  | T_Alpha of int
type string_const = { sval : string; pos : int * int; }
type char_const = { cval : char; pos : int * int; }
type int_const = { ival : int; pos : int * int; }
type float_const = { fval : float; pos : int * int; }
type id_const = { id_name : string; pos : int * int; }
type cid_const = { cid_name : string; pos : int * int; }
type bool_const = { bval : bool; pos : int * int; }
type op = { pos : int * int; }
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE
type param_status = PARDEF_COMPLETE | PARDEF_DEFINE | PARDEF_CHECK
type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool;
}
and variable_info = { mutable variable_type : typ; variable_offset : int; }
and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result : typ;
  mutable function_pstatus : param_status;
  mutable function_initquad : int;
}
and parameter_info = {
  mutable parameter_type : typ;
  mutable parameter_offset : int;
  parameter_mode : pass_mode;
}
and temporary_info = { temporary_type : typ; temporary_offset : int; }
and constructor_info = {
  constructor_type : typ;
  constructor_paramlist : typ list;
}
and entry_info =
    ENTRY_none
  | ENTRY_variable of variable_info
  | ENTRY_function of function_info
  | ENTRY_parameter of parameter_info
  | ENTRY_temporary of temporary_info
  | ENTRY_udt
  | ENTRY_constructor of constructor_info
and entry = {
  entry_id : Identifier.id;
  entry_scope : scope;
  entry_info : entry_info;
}
type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES
type binop =
    Plus
  | Fplus
  | Minus
  | Fminus
  | Times
  | Ftimes
  | Div
  | Fdiv
  | Mod
  | Power
  | Seq
  | Nseq
  | L
  | Le
  | G
  | Ge
  | Eq
  | Neq
  | And
  | Or
  | Semicolon
  | Assign
type unop = U_Plus | U_Fplus | U_Minus | U_Fminus | U_Del | U_Not
type sign = P_Plus | P_Minus
type fsign = P_Fplus | P_Fminus
type count = To | Downto
type intmb = Yesnum of int | Nonum
and ast_atom_node = {
  atom : ast_atom;
  pos : int * int;
  mutable atom_typ : typ;
  mutable entry : entry option;
}
and ast_atom =
    A_Num of int
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
  expr : ast_expr;
  pos : int * int;
  mutable expr_typ : typ;
  mutable entry : entry option;
}
and ast_expr =
    E_Binop of ast_expr_node * binop * ast_expr_node
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
and ast_clause = Clause of ast_pattern_node * ast_expr_node
and ast_pattern_node = {
  pattern : ast_pattern;
  pos : int * int;
  mutable pattern_typ : typ;
  mutable entry : entry option;
}
and ast_pattern =
    Pa_Atom of ast_pattom_node
  | Pa_Cid of string * ast_pattom_node list
and ast_pattom_node = {
  pattom : ast_pattom;
  pos : int * int;
  mutable pattom_typ : typ;
  mutable entry : entry option;
}
and ast_pattom =
    P_Sign of sign * int
  | P_Fsign of fsign * float
  | P_Num of int
  | P_Float of float
  | P_Chr of string
  | P_Bool of bool
  | P_Id of string
  | P_Cid of string
  | P_Pattern of ast_pattern
and ast_def_node = {
  def : ast_def;
  pos : int * int;
  mutable entry : entry option;
}
and ast_def =
    D_Var of (string * typ) list * ast_expr_node
  | D_Mut of (string * typ)
  | D_Array of string * typ * ast_expr_node list
and ast_stmt =
    S_Let of ast_def_node list
  | S_Rec of ast_def_node list
  | S_Type of (string * (string * typ list) list) list
val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
