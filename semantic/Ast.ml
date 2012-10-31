open Printf
open Symbol
open Identifier
open Types
open Format
open Symbtest
open Error


let rec walk_program ls =
    initSymbolTable 256;
    (* printSymbolTable (); *)
    walk_stmt_list ls

and walk_stmt_list ls = match ls with
    | []                -> ()
    | h::t              ->
          printState "Before opening" "After opening" (openScope) ();
          walk_stmt h;
          walk_stmt_list t;
          printState "Before closing" "Afterclosing" (closeScope) ();

and walk_stmt t = match t with
    | S_Let  l          -> walk_def_list l
    | S_Rec l           -> walk_recdef_list l
    | S_Type l          -> (*walk_typedef_list l*)()

and walk_def_list t = match t with
    | []                -> ()
    | h::t              ->  
          walk_def h ;
          walk_def_list t;

and walk_recdef_list t = match t with
    | []                -> ()
    | h::t              ->  
          walk_recdef_f h;
          walk_recdef_list t;
          walk_recdef h 

and walk_def t = match t with
    | D_Var (l, e)      -> 
        begin 
          match l with
            | []            -> internal "Definition cannot be empty\n";
            | (id, ty)::[]  -> 
                  let p = newVariable (id_make id) ty true in (*probably needs newscope*)
                    (* printState "Before opening" "After opening" (openScope()); *)
                    ignore p;
                    printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
                    printState "Before opening" "After opening" (hideScope !currentScope) (true);
                    let (typ, constr) = walk_expr e in
                    printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
            | (id, ty)::tl  ->
                  let p = newFunction (id_make id) true in 
                    (* printState "Before opening" "After opening" (openScope()); *)
                    printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
                    printState "Before opening" "After opening" (openScope) ();
                    walk_par_list tl p;
                    endFunctionHeader p ty;
                    (* printState "Before opening" "After opening" (openScope) (); *)
                    (* show_par_to_expr tl; *)
                    let (typ, constr) = walk_expr e in
                    printState "Before closing" "Afterclosing" (closeScope) ();
                    printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
        end
    | D_Mut (id, t) -> 
          let p = newVariable (id_make id) t true in
            ignore p;
    | D_Array (id, t, l) -> 
          let p = newVariable (id_make id) (T_Array (t, List.length l)) true in
            ignore p;
            printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
            walk_expr_list l;
            printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false)

and walk_recdef_f t = match t with
    | D_Var (l, e)      ->
        begin 
          match l with
            | [] -> printf "too many problems\n";
            | (id, ty)::[]  -> 
                  let p = newVariable (id_make id) ty true in
                    ignore p;
            | (id, ty)::tl  -> 
                let p = newFunction (id_make id) true in
                  forwardFunction p;
        end
    | D_Mut (id, t)     -> error "too many problems\n";
    | D_Array (id, t, l)  -> error "too many problems\n";

and walk_recdef t = match t with
    | D_Var (l, e)      -> 
        begin 
          match l with
            | []            -> printf "too many problems\n";
            | (id, ty)::[]  -> 
                    printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
                    let (typ, constr) = walk_expr ein 
                    printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false)
            | (id, ty)::tl  -> 
                let p = newFunction (id_make id) true in 
                    printState "Before opening" "After opening" (openScope) ();
                    walk_par_list tl p;
                    endFunctionHeader p ty;
                    let (typ, constr) = walk_expr e in
                    printState "Before closing" "Afterclosing" (closeScope) ();
        end
    | D_Mut (id, t)     -> error "too many problems\n";
    | D_Array (id, t, l)  -> error "too many problems\n";
          
and walk_par_list l p = match l with
    | []                -> ()
    | (hid,ht)::tl      -> 
          let f = newParameter (id_make hid) ht PASS_BY_VALUE p true in
            begin
            ignore f;
            walk_par_list tl p
            end

and walk_expr exp = match exp with 
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
              if (var.variable_type = T_Arrayay(_,_)) then T_Int, []
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
                        let walk_params_list param func_param =
                          match param, func_param  with
                            | [], []  -> func.function_result, []
                            | x::xs, [] -> error "Too many arguments"
                            | [], y::ys -> 
                                let tyy = match y.entry_info with
                                  | ENTRY_Parameter par_info -> par_info.parameter_type
                                  | _ -> error "Internal error"
                                in
                                let typ, constr = walk_params_list [] ys in
                                  T_Gives(tyy,typ), constr
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
          (*  TODO | E_Cid (id, l)     -> () ****)
          | E_Match (e, l)    -> 
              begin 
                let (constr,typ) = walk_expr e;
                walk_clause_list l
              end
          | E_Letin (l, e)    -> 
              begin
                openScope();
                walk_stmt l;
                let (constr,typ) = walk_expr e;
                closeScope()
              end
          | E_Atom a          -> walk_atom a

and walk_atom_list t = match t with
    | []                -> ()
    | h::t              -> 
          let (constr,typ) = walk_atom h;
          walk_atom_list t

and walk_expr_list t = match t with
    | []                -> ()
    | h::t              -> 
        let (constr,typ) = walk_expr h in
          walk_expr_list t


and walk_atom t = match t with 
  | A_Num n           -> T_Int, []
  | A_Dec f           -> T_Float, []
  | A_Chr c           -> T_Chr,  []
  | A_Str str         -> T_Array(T_Chr,1), []
  | A_Bool b          -> T_Bool, []
  (*  | A_Const con       -> TODO *)
  | A_Var v           -> 
      begin
        let s1 = lookupEntry (id_make v) LOOKUP_ALL_SCOPES true in 
          match s1.entry_info with 
            | ENTRY_variable var -> var.variable_type, []
            | ENTRY_function f ->
                let rec aux param_list =
                  match param_list with
                    | [] -> f.function_result
                    | (x::xs) ->
                        let tyx = match x.entry_info with
                          | ENTRY_Parameter par_info -> par_info.parameter_type
                          | _ -> error "Internal error\n"
                        in
                          T_Gives(tyx,aux xs)
                in
                  ((aux f.param_list), [])
            | ENTRY_parameter par -> par.parameter_type, []
      end
  | A_Par             -> T_Unit, []
  | A_Bank a          -> 
      begin 
        let tya, constra = walk_atom a in
          match tya with 
            | T_Ref(ty) -> ty, constra
            | _ -> error "Must be a reference\n"
      end
  | A_Array (a, b)    -> 
      let s1 = lookupEntry (id_make a) LOOKUP_ALL_SCOPES true in
        begin
          match s1.entry_info with 
            | ENTRY_variable arr -> 
                let tyarr = arr.variable_type in
                  begin
                    match tyarr with 
                      | T_Array(typ,dim) -> 
                          let rec walk_array_expr expr_list n acc =
                            match expr_list,n with
                              | [],0 -> acc
                              | [],_ | (x::xs), 0 -> error "array dimensions are %d\n" dim; raise Exit;
                              | (x::xs), n -> 
                                  let tyx, constrx = walk_expr x in 
                                    walk_array_expr xs (n-1) ((tyx,T_Int)::constrx@acc)
                          in
                            T_Ref(typ), walk_array_expr b dim []
                      | _ -> error "%s must be an array\n" id; raise Exit;
                  end
            | _ -> error "%s must be an array\n" id; raise Exit;
        end
  | A_Expr a          -> walk_expr a


and walk_clause_list t = match t with
  | []                -> () 
  | h::t              -> 
      walk_clause h;
      walk_clause_list t 

and walk_clause t = match t with 
  | Clause(p,e)         -> 
      walk_pattern p;
      let (constr,typ) = walk_expr e in 
      ()

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
      walk_pattom h;
      walk_pattom_list t


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
