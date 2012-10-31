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

type fsign=
    | P_Fplus
    | P_Fminus

type count =
    | To
    | Downto

type intmb =
    | Yesnum of int
    | Nonum

and ast_atom =
    | A_Num of int
    | A_Dec of float
    | A_Chr of string
    | A_Str of string
    | A_Bool of bool
    | A_Const of string
    | A_Var of string
    | A_Par 
    | A_Bank of ast_atom
    | A_Array of string * ast_expr list
    | A_Expr of ast_expr

and ast_expr =
    | E_Binop of ast_expr * binop * ast_expr
    | E_Unop of unop * ast_expr
    | E_Block of ast_expr
    | E_While of ast_expr * ast_expr
    | E_For of string * ast_expr * count * ast_expr * ast_expr
    | E_Atom of ast_atom
    | E_Dim of intmb * string
    | E_Ifthenelse of ast_expr * ast_expr * ast_expr
    | E_Ifthen of ast_expr * ast_expr
    | E_Id of string * ast_atom list
    | E_Cid of string * ast_atom list
    | E_Match of ast_expr * ast_clause list
    | E_Letin of ast_stmt * ast_expr
    | E_New of typ

and ast_clause =
      Clause of ast_pattern * ast_expr

and ast_pattern =
    | Pa_Atom of ast_pattom
    | Pa_Cid of string * ast_pattom list

and ast_pattom =
    | P_Sign of sign * int
    | P_Fsign of fsign * float
    | P_Num of int
    | P_Float of float
    | P_Chr of string
    | P_Bool of bool
    | P_Id of string
    | P_Cid of string
    | P_Pattern of ast_pattern

and ast_def =
    | D_Var of (string * typ) list * ast_expr
    | D_Mut of (string * typ) 
    | D_Array of string * typ * ast_expr list

and ast_stmt =
    | S_Let of ast_def list
    | S_Rec of ast_def list
    | S_Type of  (string * ( ( string*typ list ) list) ) list

and typ = 
    | T_Unit 
    | T_Int
    | T_Chr
    | T_Str
    | T_Bool
    | T_Float
    | T_Notype
    | T_Gives of typ * typ
    | T_Ref of typ
    | T_Array of typ * int
    | T_Id of string

let rec sizeOfType t =
   match t with
   | T_Int            -> 2
   (*| TYPE_byte           -> 1*)
   | T_Array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | T_Array (et1, sz1), T_Array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = t2
