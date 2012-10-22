open Printf
open Symbol
open Identifier
open Types
open Format

let fresh =
  let k = ref 0 in
    fun () -> incr k; TFresh !k
;;

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
        let p = newVariable (id_make id) (T_Array (t, List.length l)) true in
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
        let p = newVariable (id_make id) (T_Array (t, List.length l)) true in
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

and walk_expr cnstr exp = match exp with 
  | E_Binop (exp1, op, exp1) -> 
      begin 
        match op with 
          | Plus | Minus | Times | Div | Mod  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Int, (ty1,T_Int)::(ty2,T_Int)::cnstr1@cnstr2
          | Fplus | Fminus | Ftimes | Fdiv | Power  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Float, (ty1,T_Float)::(ty2,T_Float)::cnstr1@cnstr2
          | Seq | Nseq | Eq | Neq       -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1, ty2)::cnstr1@cnstr2 (*Must not be array or function - need to do that*)
          | L | Le | G  | Ge  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1,[T_Int, T_Float, T_Char])::(ty1, ty2)::cnstr1@cnstr2
          | And | Or      -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1,T_Bool)::(ty2, T_Bool)::cnstr1@cnstr2
          | Semicolon     -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                ty2, cnstr1@cnstr2
          | Assign        -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                ty1, (ty1,T_Ref ty2)::cnstr1@cnstr2
      end
  | E_Unop (op, exp1)     ->
      begin
        match op with
          | U_Plus | U_Minus -> 
              let ty1, cnstr1 = walk_expr exp1 in 
                T_Int, (ty1,T_Int)::cnstr1
          | U_Fplus | F_Minus  -> 
              let ty1, cnstr1 = walk_expr exp1 in 
                T_Float, (ty1, T_Float)::cnstr1
          | U_Del         -> 
              let ty1, cnstr1 = walk_expr exp1 in
                T_Unit, (ty1, T_Ref _)::cnstr1
          | U_New         ->
              let ty1, cnstr1 = walk_expr exp1 in
                (T_Ref ty1), cnstr1              (*Must not be array - need to do that*)
          | U_Not         -> 
              let ty1, cnstr1 = walk_expr exp1 in
                T_Bool, (ty1, T_Bool)::cnstr1
      end
  | E_Block exp1    -> walk_expr exp1
  | E_While (exp1, exp2)    -> 
      let ty1, cnstr1 = walk_expr exp1 in
      let ty2, cnstr2 = walk_expr exp2 in
        T_Unit, (ty1, T_Bool)::(ty2, T_Unit)::cnstr1@cnsrt2
  | E_For (id, exp1, cnt, exp2, exp3) ->
      openScope();
      let i = newVariable (id_make id) T_Int true in
        ignore i ;
        let ty1, cnstr1 = walk_expr exp1 in
        let ty2, cnstr2 = walk_expr exp2 in
        let ty3, cnstr3 = walk_expr exp3 in
          closeScope();
          T_Unit, (ty1, T_Int)::(ty2, T_Int)::(ty3, T_Unit)::cnstr1@cnstr2@cnstr3                             
  | E_Dim (a, id)      -> 
      let idEntry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        match id_entry.entry_info with
          | ENTRY_variable var -> 
              if (var.variable_type = T_Array(_,_)) then T_Int, []
              else error "Must be array"            
          | _ -> error "Must be array"
          | E_Ifthenelse (exp1, exp2, exp3)  -> (*Change ifthelse to ifthenelse*)
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
              let ty3, cnstr3 = walk_expr exp3 in
                ty2,(ty1, T_Bool)::(ty2, ty3)::cnstr1@cnstr2@cnstr3
          | E_Ifthen (e1, e2)  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Unit, (ty1, T_Bool)::(ty2, T_Unit)::cnstr1@cnstr2
          | E_Id (id, l)      -> 
              begin
                let id_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
                  match id_entry.entry_info with
                    | ENTRY_function func -> 
                        let walk_params_list param func_param acc =
                          match param, func_param  with
                            | [], []  -> func.function_result, []
                            | x::xs, [] -> error "Too many arguments"
                            | [], y::ys -> 
                                let tyy = match y.entry_info with
                                  | ENTRY_Parameter par_info -> par_info.parameter_type
                                  | _ -> error "Internal error"
                                in
                                let typ, constr = walk_params_list [] ys in
                                  T_Arrow(tyy,typ), constr
                            | x::xs, y::ys -> 
                                let tyx, constrx = walk_atom x in
                                let tyy = match y.entry_info with
                                  | ENTRY_Parameter par_info -> par_info.parameter_type
                                  | _ -> error "Internal error"
                                in 
                                let typ, cnstr = walk_params_list xs ys in
                                  typ, (tyx,tyy)::cnstr
                        in
                          walk_params_list l (func.function_paramlist) 
                    | _ -> error "Not a function" 
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
