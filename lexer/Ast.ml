open Printf

type binop =
  | Plus
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

type unop =
    | U_Plus
    | U_Fplus
    | U_Minus
    | U_Fminus
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
    | Num of int
    | Dec of float
    | Chr of char
    | Str of string
    | Bool of bool
    | Const of string
    | Var of string
    | Par 
    | Bank of ast_atom
    | Array of string * ast_expr list
    | Expr of ast_expr
    
and ast_expr =
    | Binop of ast_expr * binop * ast_expr
    | Unop of unop * ast_expr
    | Block of ast_expr
    | While of ast_expr * ast_expr
    | For of string * ast_expr * count * ast_expr * ast_expr
    | Atom of ast_atom
    | Dim of intmb * string
    | Ifthelse of ast_expr * ast_expr * ast_expr
    | Ifthe of ast_expr * ast_expr
    | E_Id of string * ast_atom list
    | E_Cid of string * ast_atom list
    | E_Match of ast_expr * ast_clause list

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
    | P_Chr of char
    | P_Bool of bool
    | P_Id of string
    | P_Cid of string
    | P_Pattern of ast_pattern

and ast_types =
    | T_unit 
    | T_int
    | T_chr
    | T_bool
    | T_float
    | T_notype
    | T_gives of ast_types * ast_types
    | T_ref of ast_types
    | T_arr of ast_types * int
    | T_id of string

and ast_def =
    | D_var of (string * ast_types) list * ast_expr
    | D_mut of (string * ast_types) 
    | D_arr of string * ast_types * ast_expr list

and ast_letdef = 
    | L_Let of ast_def list
    | L_Rec of ast_def list

and ast_typedef =
    | T_Typedef of (string * ( ( string*ast_types list ) list) ) list  

let rec pp_expr out t = match t with   
    | Binop (a, op, b)  -> 
        begin 
          match op with 
            | Plus          -> fprintf out "Plus (%a, %a)" pp_expr a pp_expr b
            | Fplus         -> fprintf out "Minus (%a, %a)" pp_expr a pp_expr b   
            | Minus         -> fprintf out "Fplus (%a, %a)" pp_expr a pp_expr b   
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
    | Unop (a, b)       ->
        begin
          match a with
            | U_Plus        -> fprintf out "Unplus (%a)" pp_expr b 
            | U_Fplus       -> fprintf out "Unfplus (%a)" pp_expr b  
            | U_Minus       -> fprintf out "Unminus (%a)" pp_expr b  
            | U_Fminus      -> fprintf out "Unfminus (%a)" pp_expr b  
            | U_Del         -> fprintf out "Delete (%a)" pp_expr b 
            | U_Not         -> fprintf out "Not (%a)" pp_expr b
        end
    | Block a           -> fprintf out "Block (%a)" pp_expr a 
    | While (a, b)      -> fprintf out "While (%a,%a)" pp_expr a pp_expr b
    | For (id, e1, cnt, e2, e3) ->
        begin
          match cnt with
            | To            -> fprintf out "For %s = %a to %a do %a done" id pp_expr e1 pp_expr e2 pp_expr e3
            | Downto        -> fprintf out "For %s = %a downto %a do %a done" id pp_expr e1 pp_expr e2 pp_expr e3
        end
    | Dim (a, b)        -> 
        begin
          match a with
            | Nonum         -> fprintf out "Dim %s" b
            | Yesnum a      -> fprintf out "Dim %d %s" a b
        end
    | Ifthelse (e1, e2, e3)  
                        -> fprintf out "if (%a) then (%a) else (%a)" pp_expr e1 pp_expr e2 pp_expr e3
    | Ifthe (e1, e2)    -> fprintf out "if (%a) then (%a)" pp_expr e1 pp_expr e2
    | E_Id (id, l)      -> fprintf out "%s [%a]" id pp_atom_list l
    | E_Cid (id, l)     -> fprintf out "%s [%a]" id pp_atom_list l
    | E_Match (e, l)    -> fprintf out "Match (%a) With (%a)" pp_expr e pp_clause_list l
    | Atom a            -> fprintf out "%a" pp_atom a

and pp_expr_list out t = match t with
    | []    -> ()
    | h::[] -> fprintf out "%a" pp_expr h 
    | h::t  -> fprintf out "%a, %a" pp_expr h pp_expr_list t

and pp_atom out t = match t with 
    | Num n             -> fprintf out "%d" n
    | Dec f             -> fprintf out "%f" f
    | Chr c             -> fprintf out "%c" c
    | Str str           -> fprintf out "%s" str
    | Bool b            -> fprintf out "%B" b
    | Const con         -> fprintf out "%s" con
    | Var v             -> fprintf out "%s" v
    | Par               -> fprintf out "()"
    | Bank a            -> fprintf out "!( %a )" pp_atom a
    | Array (a, b)      -> fprintf out "%s[%a]" a pp_expr_list b
    | Expr a            -> fprintf out "(%a)" pp_expr a

and pp_atom_list out t = match t with
    | []    -> ()
    | h::[] -> fprintf out "%a" pp_atom h 
    | h::t  -> fprintf out "%a, %a" pp_atom h pp_atom_list t

and pp_clause out t = match t with 
  | Clause(p,e) -> fprintf out "clause(%a, %a)" pp_pattern p pp_expr e

and pp_pattern out p = match p with
    | Pa_Atom a          -> fprintf out "%a" pp_pattom a
    | Pa_Cid (cid, l)    -> fprintf out "%s [%a]" cid pp_pattom_list l

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
    | P_Chr c           -> fprintf out "%c" c
    | P_Bool b          -> fprintf out "%B" b
    | P_Id id           -> fprintf out "%s" id
    | P_Cid cid         -> fprintf out "%s" cid
    | P_Pattern p       -> fprintf out "%a" pp_pattern p

and pp_pattom_list out t = match t with
    | []    -> ()
    | h::[] -> fprintf out "%a" pp_pattom h 
    | h::t  -> fprintf out "%a, %a" pp_pattom h pp_pattom_list t

and pp_clause_list out t = match t with
    | []    -> () 
    | h::[] -> fprintf out "%a" pp_clause h 
    | h::t  -> fprintf out "%a, %a" pp_clause h pp_clause_list t

and pp_types out t = match t with
    | T_unit            -> fprintf out "unit"
    | T_int             -> fprintf out "int"
    | T_chr             -> fprintf out "chr"
    | T_bool            -> fprintf out "bool"
    | T_float           -> fprintf out "float"
    | T_gives (t1, t2)  -> fprintf out "gives(%a, %a)" pp_types t1 pp_types t2
    | T_ref t           -> fprintf out "ref(%a)" pp_types t
    | T_arr (t, d)      -> fprintf out "array[%d] of %a" d pp_types t
    | T_id id           -> fprintf out "id(%s)" id
    | T_notype          -> fprintf out "kalh diaskedash"

and pp_def out t = match t with
    | D_var (l, e)      -> fprintf out "(%a)=(%a)" pp_par_list l pp_expr e
    | D_mut (id, t)     -> fprintf out "mutable(%s,%a)" id pp_types t
    | D_arr (id, t, l ) -> fprintf out "array(%s,%a,[%a])" id pp_types t pp_expr_list l

and pp_par_list out l = match l with
    | []                -> ()
    | (id,t)::[]        -> fprintf out "(%s,%a)" id pp_types t
    | (hid,ht)::tl      -> fprintf out "(%s,%a), %a" hid pp_types ht pp_par_list tl

and pp_letdef out t = match t with
    | L_Let  l          -> fprintf out "Let(%a)" pp_def_list l
    | L_Rec l           -> fprintf out "Rec(%a)" pp_def_list l

and pp_def_list out t = match t with
    | []                -> ()
    | h::[] -> fprintf out "%a" pp_def h 
    | h::t  -> fprintf out "%a, %a" pp_def h pp_def_list t

and pp_typedef out t = match t with
    | T_Typedef []                -> ()
    | T_Typedef [(id, lst)]   -> fprintf out "type %s = ( %a )" id pp_cidtype_list lst
    | T_Typedef (id,lst)::t     -> fprintf out "type %s = ( %a ) and  %a " id pp_cidtype_list lst  pp_typedef_list t

and pp_cidtype_list out t = match t with
    | []                -> ()
    | (cid, tplst)::[]  -> fprintf out "%s of ( %a )" cid pp_type_list tplst
    | (cid, tplst)::t   -> fprintf out "%s of ( %a ) | %a" cid pp_type_list tplst pp_cidtype_list t

and pp_type_list out t = match t with
    | []                -> ()
    | h::[]             -> fprintf out "%a" pp_types h 
    | h::t              -> fprintf out "%a %a" pp_types h pp_type_list t

                 
