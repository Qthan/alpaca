open Types
open SymbTypes

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

and ast_atom_node = {
  atom                   : ast_atom;
  atom_pos               : (int * int);
  mutable atom_typ       : typ;
  mutable atom_entry     : entry option;
  mutable atom_tail      : bool
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
    | A_Bang of ast_atom_node
    | A_Array of string * ast_expr_node list
    | A_Expr of ast_expr_node
    | A_None

and ast_expr_node = {
  expr                   : ast_expr;
  expr_pos               : (int * int);
  mutable expr_typ       : typ;
  mutable expr_entry     : entry option;
  mutable expr_tail      : bool
}

and ast_expr =
    | E_Binop of ast_expr_node * binop * ast_expr_node
    | E_Unop of unop * ast_expr_node
    | E_Block of ast_expr_node
    | E_While of ast_expr_node * ast_expr_node
    | E_For of string * ast_expr_node * count * ast_expr_node * ast_expr_node
    | E_Atom of ast_atom_node
    | E_Dim of int option * string
    | E_Ifthenelse of ast_expr_node * ast_expr_node * ast_expr_node
    | E_Ifthen of ast_expr_node * ast_expr_node
    | E_Id of string * ast_atom_node list
    | E_Cid of string * ast_atom_node list
    | E_Match of ast_expr_node * ast_clause list
    | E_Letin of ast_stmt * ast_expr_node
    | E_New of typ
    | E_None (* dummy expr*)

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
    

