open Printf
open Symbol
open Identifier
open Types
open Format

let rec walk_program ls =
  begin 
    initSymbolTable 256;
    walk_stmt_list ls
  end

and walk_stmt_list ls = match ls with
    | []                -> ()
    | h::t              ->
        begin
          openScope();
          walk_stmt h;
          walk_stmt_list t;
          closeScope();
        end

and walk_stmt t = match t with
    | S_Let  l          -> walk_def_list l
    | S_Rec l           -> 
        begin
          walk_recdef_list l
        end
    | S_Type l          -> (*walk_typedef_list l*)()

and walk_def_list t = match t with
    | []                -> ()
    | h::t              ->  
        begin 
          walk_def h ;
          walk_def_list t;
        end

and walk_recdef_list t = match t with
    | []                -> ()
    | h::t              ->  
        begin 
          walk_recdef_f h;
          walk_recdef_list t;
          walk_recdef h 
        end

and walk_def t = match t with
    | D_Var (l, e)      -> 
        begin 
          match l with
            | []            -> printf "too many problems\n";
            | (id, ty)::[]  -> 
                  let p = newVariable (id_make id) ty true in (*probably needs newscope*)
                    ignore p; 
                    hideScope !currentScope true;
                    walk_expr e;
                    hideScope !currentScope false;
            | (id, ty)::tl  -> 
                  let p = newFunction (id_make id) true in 
                    walk_par_list tl p;
                    endFunctionHeader p ty;
                    hideScope !currentScope true;
                    openScope();
                    expr_par tl;
                    walk_expr e;
                    closeScope();
                    hideScope !currentScope false
        end
    | D_Mut (id, t)     -> 
        begin
          let p = newVariable (id_make id) t true in
            ignore p;
        end
    | D_Arr (id, t, l)  -> 
        begin
          let p = newVariable (id_make id) (T_Arr (t, List.length l)) true in
            ignore p;
            hideScope !currentScope true;
            walk_expr_list l;
            hideScope !currentScope false
        end

and walk_recdef_f t = match t with
    | D_Var (l, e)      ->
        begin 
          match l with
            | []            -> printf "too many problems\n";
            | (id, ty)::[]  -> 
                  let p = newVariable (id_make id) ty true in
                    ignore p;
            | (id, ty)::tl  -> 
                  let p = newFunction (id_make id) true in 
                    walk_par_list tl p;
                    endFunctionHeader p ty
        end
    | D_Mut (id, t)     -> 
        begin
          let p = newVariable (id_make id) t true in
            ignore p;
        end
    | D_Arr (id, t, l)  -> 
        begin
          let p = newVariable (id_make id) (T_Arr (t, List.length l)) true in
            ignore p;
        end

and walk_recdef t = match t with
    | D_Var (l, e)      -> 
        begin 
          match l with
            | []            -> printf "too many problems\n";
            | (id, ty)::[]  -> 
                    hideScope !currentScope true;
                    walk_expr e;
                    hideScope !currentScope false
            | (id, ty)::tl  -> 
                    openScope();
                    expr_par tl;
                    walk_expr e;
                    closeScope();
        end
    | D_Mut (id, t)     -> ()
    | D_Arr (id, t, l)  -> 
        begin
            walk_expr_list l
        end
          
and expr_par l = match l with
    | []                -> ()
    | (hid, ht)::t      -> 
        begin
          let p = newVariable (id_make hid) ht true in
            ignore p;
            expr_par t
        end

and walk_par_list l p = match l with
    | []                -> ()
    | (hid,ht)::tl      -> 
          let f = newParameter (id_make hid) ht PASS_BY_VALUE p true in
            begin
            ignore f;
            walk_par_list tl p
            end

and walk_expr t = match t with   
    | E_Binop (a, op, b)-> 
        begin 
          match op with 
            | Plus          -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Fplus         -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Minus         -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Fminus        -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Times         -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Ftimes        -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Div           -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Fdiv          -> 
                begin
                  walk_expr a;
                  walk_expr b    
                end
            | Mod           -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Power         -> 
                begin
                  walk_expr a;
                  walk_expr b   
                end
            | Seq           -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Nseq          -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | L             -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Le            -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | G             -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Ge            -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Eq            -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Neq           -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | And           -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Or            -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
            | Semicolon     -> 
                begin
                  walk_expr a;
                  walk_expr b     
                end
            | Assign        -> 
                begin
                  walk_expr a;
                  walk_expr b
                end
        end
    | E_Unop (a, b)     ->
        begin
          match a with
            | U_Plus        -> walk_expr b 
            | U_Fplus       -> walk_expr b  
            | U_Minus       -> walk_expr b  
            | U_Fminus      -> walk_expr b  
            | U_Del         -> walk_expr b 
            | U_Not         -> walk_expr b
        end
    | E_Block a         -> walk_expr a 
    | E_While (a, b)    -> 
        begin
          walk_expr a;
          walk_expr b
        end
    | E_For (id, e1, cnt, e2, e3) ->
        begin
          match cnt with
            | To            -> 
                begin
                  openScope();
                  let i = newVariable (id_make id) T_Int true in
                    ignore i ;
                  walk_expr e1 ;
                  walk_expr e2 ;
                  walk_expr e3 ;
                  closeScope()
                end
            | Downto        -> 
                begin
                  openScope();
                  let i = newVariable (id_make id) T_Int true in
                    ignore i ;
                  walk_expr e1 ;
                  walk_expr e2 ;
                  walk_expr e3 ;
                  closeScope()
                end
        end
    | E_Dim (a, b)      -> 
        begin
          match a with
            | Nonum         -> ()
            | Yesnum a      -> ()
        end
    | E_Ifthelse (e1, e2, e3)  
                        -> 
        begin 
          walk_expr e1;
          walk_expr e2;
          walk_expr e3
        end
    | E_Ifthe (e1, e2)  -> 
        begin 
          walk_expr e1;
          walk_expr e2;
        end
    | E_Id (id, l)      -> 
        begin
          let s1 = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
            ignore s1 ;
            walk_atom_list l 
        end
    | E_Cid (id, l)     -> ()
    | E_Match (e, l)    -> 
        begin 
          walk_expr e;
          walk_clause_list l
        end
    | E_Letin (l, e)    -> 
        begin
          openScope();
          walk_stmt l;
          walk_expr e;
          closeScope()
        end
    | E_New t           -> ()
    | E_Atom a          -> walk_atom a

and walk_atom_list t = match t with
    | []                -> ()
    | h::t              -> 
        begin
          walk_atom h;
          walk_atom_list t
        end

and walk_expr_list t = match t with
    | []                -> ()
    | h::t              -> 
        begin
          walk_expr h;
          walk_expr_list t
        end


and walk_atom t = match t.atom with 
    | A_Num n           -> ()
    | A_Dec f           -> ()
    | A_Chr c           -> ()
    | A_Str str         -> ()
    | A_Bool b          -> ()
    | A_Const con       -> ()
    | A_Var v           -> let s1 = lookupEntry (id_make v) LOOKUP_CURRENT_SCOPE true in ignore s1
    | A_Par             -> ()
    | A_Bank a          -> walk_atom a
    | A_Array (a, b)    -> let s1 = lookupEntry (id_make a) LOOKUP_ALL_SCOPES true in ignore s1
    | A_Expr a          -> walk_expr a


and walk_clause_list t = match t with
    | []                -> () 
    | h::t              -> 
        begin
          walk_clause h;
          walk_clause_list t 
        end

and walk_clause t = match t with 
  | Clause(p,e)         -> 
      begin
        walk_pattern p;
        walk_expr e
      end

and walk_pattern p = match p with
    | Pa_Atom a         -> walk_pattom a
    | Pa_Cid (cid, l)   -> ()(*to be done*)

and walk_pattom t = match t with
    | P_Sign(op, num)   ->
        begin
          match op with 
            | P_Plus        -> ()
            | P_Minus       -> ()
        end
    | P_Fsign(op, num)  ->
        begin
          match op with 
            | P_Fplus       -> ()
            | P_Fminus      -> ()
        end
    | P_Num n           -> ()
    | P_Float f         -> ()
    | P_Chr c           -> ()
    | P_Bool b          -> ()
    | P_Id id           -> let s1 = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in ignore s1
    | P_Cid cid         -> () (*to be done*)
    | P_Pattern p       -> walk_pattern p

and walk_pattom_list t = match t with
    | []                -> ()
    | h::t              -> 
        begin
          walk_pattom h;
          walk_pattom_list t
        end





(*
check_types t = match t with
  | (A_Num n , A_Num n)     -> true
  | (A_NUM n, A_Var v)      -> 
      begin
        let s1 = lookupEntry (id_make v) LOOKUP_CURRENT_SCOPE true in 
          if s1.entry_info.constructor_type = T_Int then true
          else false
      end
  | (A_Var, A_Num)          -> 
      begin
        let s1 = lookupEntry (id_make "con") LOOKUP_CURRENT_SCOPE true in 
          if s1.entry_info.constructor_type = T_Int then true
          else false
      end
  | (A_Dec n , A_Dec n)     -> true
  | (A_Dec n, A_Var v)      -> 
      begin
        let s1 = lookupEntry (id_make v) LOOKUP_CURRENT_SCOPE true in 
          if s1.entry_info.constructor_type = T_Float then true
          else false
      end
  | (A_Var, A_Dec)          -> 
      begin
        let s1 = lookupEntry (id_make "con") LOOKUP_CURRENT_SCOPE true in 
          if s1.entry_info.constructor_type = T_Float then true
          else false *)
