open Printf

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

type ast_atom =
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
  | E_Ifthelse of ast_expr * ast_expr * ast_expr
  | E_Ifthe of ast_expr * ast_expr
  | E_Id of string * ast_atom list
  | E_Cid of string * ast_atom list
  | E_Match of ast_expr * ast_clause list
  | E_Letin of ast_stmt * ast_expr
  | E_New of ast_types

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

and ast_types =
  | T_Unit 
  | T_Int
  | T_Chr
  | T_Bool
  | T_Float
  | T_Notype
  | T_Arrow of ast_types * ast_types
  | T_Ref of ast_types
  | T_Array of ast_types * int
  | T_Id of string

and ast_def =
  | D_Var of (string * ast_types) list * ast_expr
  | D_Mut of (string * ast_types) 
  | D_Array of string * ast_types * ast_expr list

and ast_stmt =
  | S_Let of ast_def list
  | S_Rec of ast_def list
  | S_Type of  (string * ( ( string*ast_types list ) list) ) list

let rec pp_expr out t = match t with   
  | E_Binop (a, op, b)-> 
    begin 
      match op with 
        | Plus          -> fprintf out "Plus (%a, %a)" pp_expr a pp_expr b
        | Fplus         -> fprintf out "Fplus (%a, %a)" pp_expr a pp_expr b   
        | Minus         -> fprintf out "minus (%a, %a)" pp_expr a pp_expr b   
        | Fminus        -> fprintf out "Fminus (%a, %a)" pp_expr a pp_expr b   
        | Times         -> fprintf out "Times (%a, %a)" pp_expr a pp_expr b   
        | Ftimes        -> fprintf out "Ftimes (%a, %a)" pp_expr a pp_expr b   
        | Div           -> fprintf out "Div (%a, %a)" pp_expr a pp_expr b
        | Fdiv          -> fprintf out "Fdiv (%a, %a)" pp_expr a pp_expr b    
        | Mod           -> fprintf out "Mod (%a, %a)" pp_expr a pp_expr b
        | Power         -> fprintf out "Power (%a, %a)" pp_expr a pp_expr b   
        | Seq           -> fprintf out "Seq (%a, %a)" pp_expr a pp_expr b
        | Nseq          -> fprintf out "Nseq (%a, %a)" pp_expr a pp_expr b
        | L             -> fprintf out "L (%a, %a)" pp_expr a pp_expr b
        | Le            -> fprintf out "G (%a, %a)" pp_expr a pp_expr b
        | G             -> fprintf out "Le (%a, %a)" pp_expr a pp_expr b
        | Ge            -> fprintf out "Ge (%a, %a)" pp_expr a pp_expr b
        | Eq            -> fprintf out "Eq (%a, %a)" pp_expr a pp_expr b
        | Neq           -> fprintf out "Neq (%a, %a)" pp_expr a pp_expr b
        | And           -> fprintf out "And (%a, %a)" pp_expr a pp_expr b
        | Or            -> fprintf out "Or (%a, %a)" pp_expr a pp_expr b
        | Semicolon     -> fprintf out "Smcolon (%a, %a)" pp_expr a pp_expr b     
        | Assign        -> fprintf out "Assign (%a, %a)" pp_expr a pp_expr b
    end
  | E_Unop (a, b)     ->
    begin
      match a with
        | U_Plus        -> fprintf out "Unplus (%a)" pp_expr b 
        | U_Fplus       -> fprintf out "Unfplus (%a)" pp_expr b  
        | U_Minus       -> fprintf out "Unminus (%a)" pp_expr b  
        | U_Fminus      -> fprintf out "Unfminus (%a)" pp_expr b  
        | U_Del         -> fprintf out "Delete (%a)" pp_expr b 
        | U_Not         -> fprintf out "Not (%a)" pp_expr b
    end
  | E_Block a         -> fprintf out "Block (%a)" pp_expr a 
  | E_While (a, b)    -> fprintf out "While (%a,%a)" pp_expr a pp_expr b
  | E_For (id, e1, cnt, e2, e3) ->
    begin
      match cnt with
        | To            -> fprintf out "For %s = %a to %a do %a done" id pp_expr e1 pp_expr e2 pp_expr e3
        | Downto        -> fprintf out "For %s = %a downto %a do %a done" id pp_expr e1 pp_expr e2 pp_expr e3
    end
  | E_Dim (a, b)      -> 
    begin
      match a with
        | Nonum         -> fprintf out "Dim %s" b
        | Yesnum a      -> fprintf out "Dim %d %s" a b
    end
  | E_Ifthelse (e1, e2, e3)  
    -> fprintf out "if (%a) then (%a) else (%a)" pp_expr e1 pp_expr e2 pp_expr e3
  | E_Ifthe (e1, e2)  -> fprintf out "if (%a) then (%a)" pp_expr e1 pp_expr e2
  | E_Id (id, l)      -> fprintf out "%s %a" id pp_atom_list l
  | E_Cid (id, l)     -> fprintf out "%s %a" id pp_atom_list l
  | E_Match (e, l)    -> fprintf out "Match (%a) With (%a)" pp_expr e pp_clause_list l
  | E_Letin (l, e)    -> fprintf out "Let %a in (%a)" pp_stmt l pp_expr e
  | E_New t           -> fprintf out "New(%a)" pp_types t
  | E_Atom a          -> fprintf out "%a" pp_atom a

and pp_expr_list out t = match t with
  | []                -> ()
  | h::[]             -> fprintf out "%a" pp_expr h 
  | h::t              -> fprintf out "%a, %a" pp_expr h pp_expr_list t

and pp_atom out t = match t with 
  | A_Num n           -> fprintf out "%d" n
  | A_Dec f           -> fprintf out "%f" f
  | A_Chr c           -> fprintf out "%s" c
  | A_Str str         -> fprintf out "%s" str
  | A_Bool b          -> fprintf out "%B" b
  | A_Const con       -> fprintf out "%s" con
  | A_Var v           -> fprintf out "%s" v
  | A_Par             -> fprintf out "()"
  | A_Bank a          -> fprintf out "!( %a )" pp_atom a
  | A_Array (a, b)    -> fprintf out "%s[%a]" a pp_expr_list b
  | A_Expr a          -> fprintf out "(%a)" pp_expr a

and pp_atom_list out t = match t with
  | []                -> ()
  | h::[]             -> fprintf out "%a" pp_atom h 
  | h::t              -> fprintf out "%a, %a" pp_atom h pp_atom_list t

and pp_clause out t = match t with 
  | Clause(p,e)         -> fprintf out "clause(%a, %a)" pp_pattern p pp_expr e

and pp_pattern out p = match p with
  | Pa_Atom a         -> fprintf out "%a" pp_pattom a
  | Pa_Cid (cid, l)   -> fprintf out "%s [%a]" cid pp_pattom_list l

and pp_pattom out t = match t with
  | P_Sign(op, num)   ->
    begin
      match op with 
        | P_Plus        -> fprintf out "Plus(%d)" num 
        | P_Minus       -> fprintf out "Minus(%d)" num
    end
  | P_Fsign(op, num)  ->
    begin
      match op with 
        | P_Fplus       -> fprintf out "Fplus(%f)" num
        | P_Fminus      -> fprintf out "Fminus(%f)" num
    end
  | P_Num n           -> fprintf out "%d" n
  | P_Float f         -> fprintf out "%f" f
  | P_Chr c           -> fprintf out "%s" c
  | P_Bool b          -> fprintf out "%B" b
  | P_Id id           -> fprintf out "%s" id
  | P_Cid cid         -> fprintf out "%s" cid
  | P_Pattern p       -> fprintf out "%a" pp_pattern p

and pp_pattom_list out t = match t with
  | []                -> ()
  | h::[]             -> fprintf out "%a" pp_pattom h 
  | h::t              -> fprintf out "%a, %a" pp_pattom h pp_pattom_list t

and pp_clause_list out t = match t with
  | []                -> () 
  | h::[]             -> fprintf out "%a" pp_clause h 
  | h::t              -> fprintf out "%a, %a" pp_clause h pp_clause_list t

and pp_types out t = match t with
  | T_Unit            -> fprintf out "unit"
  | T_Int             -> fprintf out "int"
  | T_Chr             -> fprintf out "chr"
  | T_Bool            -> fprintf out "bool"
  | T_Float           -> fprintf out "float"
  | T_Arrow (t1, t2)  -> fprintf out "gives(%a, %a)" pp_types t1 pp_types t2
  | T_Ref t           -> fprintf out "ref(%a)" pp_types t
  | T_Array (t, d)      -> fprintf out "array[%d] of %a" d pp_types t
  | T_Id id           -> fprintf out "id(%s)" id
  | T_Notype          -> fprintf out "kalh diaskedash"

and pp_types_list out l = match l with 
  | []                -> ()
  | h::[]             -> fprintf out "%a" pp_types h 
  | h::t              -> fprintf out "%a, %a" pp_types h pp_types_list t

and pp_def out t = match t with
  | D_Var (l, e)      -> fprintf out "(%a)=(%a)" pp_par_list l pp_expr e
  | D_Mut (id, t)     -> fprintf out "mutable(%s,%a)" id pp_types t
  | D_Array (id, t, l)  -> fprintf out "array(%s,%a,[%a])" id pp_types t pp_expr_list l

and pp_par_list out l = match l with
  | []                -> ()
  | (id,t)::[]        -> fprintf out "(%s,%a)" id pp_types t
  | (hid,ht)::tl      -> fprintf out "(%s,%a), %a" hid pp_types ht pp_par_list tl

and pp_def_list out t = match t with
  | []                -> ()
  | h::[]             -> fprintf out "%a" pp_def h 
  | h::t              -> fprintf out "%a, %a" pp_def h pp_def_list t

and pp_constrbar_list out l = match l with
  | []                -> ()
  | (cid,t)::[]       -> fprintf out "(%s,%a)" cid pp_types_list t
  | (hcid,ht)::tl     -> fprintf out "(%s,%a), %a" hcid pp_types_list ht pp_constrbar_list tl

and pp_typedef_list out l = match l with
  | []                -> ()
  | (id, cbl)::[]     -> fprintf out "(%s,(%a))" id pp_constrbar_list cbl
  | (hid, hcbl)::tl   -> fprintf out "(%s,(%a)), %a" hid pp_constrbar_list hcbl pp_typedef_list tl

and pp_stmt out t = match t with
  | S_Let  l          -> fprintf out "Let(%a)\n" pp_def_list l
  | S_Rec l           -> fprintf out "Rec(%a)\n" pp_def_list l
  | S_Type l          -> fprintf out "Type(%a)\n" pp_typedef_list l

and run out ls = List.iter (fprintf out "Program %a" pp_stmt) ls
