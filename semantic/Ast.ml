(*


             ________________________________________________,--.__
  --    ,--""                                               '\     '\
       /  "                                                   \     '\
     ,/                                                       '\     |
     | "   "   "                                                '\,  /
     |           " , =__________________________________________,--""
   - |  "    "    /"/'    
     \  "      ",/ /    
      \   ",",_/,-'       
   -- -'-;.__:-'
   
*)


open Printf
open Symbol
open Identifier
open Types
open Format
open Symbtest
open Error
open Typeinf

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
  | S_Rec l           -> 
      List.iter walk_recdef_names l;      
      let cnstr = walk_recdef_list l in
        updateSymbolRec l (unify cnstr);
  | S_Type l          -> walk_typedef_list l

and walk_def_list t = match t with
  | []                -> ()
  | h::t              ->  
      walk_def h ;
      walk_def_list t;

and walk_recdef_list t  = match t with
  | []                -> []
  | h::t              ->
      walk_recdef_params h;
      let cntr = walk_recdef_list t in
      (walk_recdef h)@cntr  

and walk_def t = match t with
  | D_Var (l, e)      ->
      begin
        match l with
          | []            -> internal "Definition cannot be empty";
          | (id, ty)::[]  ->
                let new_ty = refresh ty in
                (*printState "Before hiding" "After hiding" (hideScope !currentScope) (true); --probably not needed *)
                let (typ, constr) = walk_expr e in
                let p = newVariable (id_make id) new_ty true in
                  ignore p;
                  updateSymbol [(id,ty)] (unify ((new_ty,typ) :: constr));  
                  (* printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false); --probably not needed *)

          | (id, ty)::tl  ->
              let p = newFunction (id_make id) true in 
                (* printState "Before opening" "After opening" (openScope()); *)
                printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
                printState "Before opening" "After opening" (openScope) ();
                walk_par_list tl p;
                let new_ty = refresh ty in
                endFunctionHeader p new_ty;
                (* printState "Before opening" "After opening" (openScope) (); *)
                (* show_par_to_expr tl; *)
                let (typ, constr) = walk_expr e in 
                  printState "Before closing" "Afterclosing" (closeScope) ();
                  printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
                  updateSymbol [(id,ty)] (unify ((new_ty,typ) :: constr)); 
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

and walk_recdef_names t = match t with
  | D_Var (l, e)       ->
      begin 
        match l with
          | [] -> internal "Definition cannot be empty";
          | (id, ty)::[]  ->
              let new_ty = refresh ty in
              let p = newVariable (id_make id) new_ty true in
                ignore p;
          | (id, ty)::tl  -> 
              let new_ty = refresh ty in
              let p = newFunction (id_make id) true in
                setType p new_ty;
                forwardFunction p
      end
  | D_Mut (id, t)       -> error "Mutable cannot be rec\n";
  | D_Array (id, t, l)  -> error "Array cannot be rec\n";

and walk_recdef_params t = match t with
  | D_Var(l, e)  ->
    begin 
        match l with
          | [] -> printf "too many problems\n";
          | (id, ty)::[]  -> ()
          | (id, ty)::tl  -> 
              let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
              let new_ty = getType p in
              printState "Before opening" "After opening" (openScope) ();
              walk_par_list tl p;
              printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
              endFunctionHeader p new_ty;
      end
  | D_Mut (id, t)       -> error "Mutable cannot be rec\n";
  | D_Array (id, t, l)  -> error "Array cannot be rec\n";   

and walk_recdef t = match t with
  | D_Var (l, e)      -> 
      begin 
        match l with
          | []            -> error "too many problems\n"; raise Exit
          | (id, ty) :: []  -> 
              printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
              let (typ, constr) = walk_expr e in 
                printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
                ((getType ( lookupEntry (id_make id) LOOKUP_ALL_SCOPES true), typ) :: constr)
          | (id, ty) :: tl  -> 
              (* let p = newFunction (id_make id) true in *) 
              (*   printState "Before opening" "After opening" (openScope) (); *)
              (*   walk_par_list tl p; *)
              (*   endFunctionHeader p new_ty; *)
                let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
                let new_ty = getType p in
                printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
                let (typ, constr) = walk_expr e in
                  printState "Before closing" "Afterclosing" (closeScope) ();
                 ((new_ty, typ) :: constr)
      end
  | D_Mut (id, t)       -> error "Mutable cannot be rec\n"; raise Exit;
  | D_Array (id, t, l)  -> error "Array cannot be rec\n";   raise Exit;

and walk_par_list l p = match l with
  | []                -> ()
  | (hid,ht)::tl      -> 
      let new_ty = refresh ht in
      let f = newParameter (id_make hid) new_ty PASS_BY_VALUE p true in
        begin
          ignore f;
          walk_par_list tl p
        end

(* Walks user defined types and initially registers the types' names into the
 * symbol table (used for mutually recursive defined types). Afterwards it
 * walks the constructors list and if they are well defined proceeds with
 * registering them as well. *)

 and walk_typedef_list l = match l with
  | [] -> internal "Type definition cannot be empty"
  | l -> 
      List.iter (fun (id, _) -> ignore (newUdt (id_make id) true)) l; (* Adds user defined types names to symbol table *)
      let walk_constructor tid cid types_list = 
        List.iter (function 
                     | T_Id id -> let entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
                         begin
                           match entry.entry_info with
                             | ENTRY_udt -> ()
                             | _ -> error "Constructor %s parameters must be of a valid type\n" cid; raise Exit;
                         end
                     | _ -> ()) types_list;
        let c = newConstructor (id_make cid) (T_Id tid) types_list true in
          ignore c;
      in
      List.iter (fun (id, constructors_list) -> 
                   List.iter (fun (cid,types_list) -> walk_constructor id cid types_list) constructors_list) l

and walk_expr exp = match exp with 
  | E_Binop (exp1, op, exp2) -> 
      begin 
        match op with 
          | Plus | Minus | Times | Div | Mod  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Int, (ty1,T_Int) :: (ty2,T_Int) :: cnstr1@cnstr2
          | Fplus | Fminus | Ftimes | Fdiv | Power  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Float, (ty1,T_Float) :: (ty2,T_Float) :: cnstr1@cnstr2
          | Seq | Nseq | Eq | Neq       -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1, ty2 ) :: cnstr1 @ cnstr2 (*Must not be array or function - need to do that*)
          | L | Le | G  | Ge  -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1,T_Int) :: (ty1, ty2) :: cnstr1 @ cnstr2 (*ty1 = T_Int | T_Float | T_Char, using T_Int for the moment, type typ_inf = Ord | Typ of typ or [typ] for all*)
          | And | Or      -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                T_Bool, (ty1,T_Bool) :: (ty2, T_Bool) :: cnstr1 @ cnstr2
          | Semicolon     -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                ty2, cnstr1@cnstr2
          | Assign        -> 
              let ty1, cnstr1 = walk_expr exp1 in
              let ty2, cnstr2 = walk_expr exp2 in
                ty1, (ty1,T_Ref ty2) :: cnstr1 @ cnstr2
      end
  | E_Unop (op, exp1)     ->
      begin
        match op with
          | U_Plus | U_Minus -> 
              let ty1, cnstr1 = walk_expr exp1 in 
                T_Int, (ty1, T_Int) :: cnstr1
          | U_Fplus | U_Fminus  -> 
              let ty1, cnstr1 = walk_expr exp1 in 
                T_Float, (ty1, T_Float) :: cnstr1
          | U_Del         -> 
              let ty1, cnstr1 = walk_expr exp1 in
                T_Unit, (ty1, T_Ref (T_Notype)) :: cnstr1   (*argument for T_Ref constructor??*)
          | U_Not         -> 
              let ty1, cnstr1 = walk_expr exp1 in
                T_Bool, (ty1, T_Bool) :: cnstr1
      end
  | E_Block exp1    -> walk_expr exp1
  | E_While (exp1, exp2)    -> 
      let ty1, cnstr1 = walk_expr exp1 in
      let ty2, cnstr2 = walk_expr exp2 in
        T_Unit, (ty1, T_Bool) :: (ty2, T_Unit) :: cnstr1 @ cnstr2
  | E_For (id, exp1, cnt, exp2, exp3) ->
      openScope();
      let i = newVariable (id_make id) T_Int true in
        ignore i ;
        let ty1, cnstr1 = walk_expr exp1 in
        let ty2, cnstr2 = walk_expr exp2 in
        let ty3, cnstr3 = walk_expr exp3 in
          closeScope();
          T_Unit, (ty1, T_Int) :: (ty2, T_Int) :: (ty3, T_Unit) :: cnstr1 @ cnstr2 @ cnstr3                             
  | E_Dim (a, id)      ->
      begin
        let id_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
          match id_entry.entry_info with
            | ENTRY_variable var -> 
                begin
                  match var.variable_type with
                    | T_Array (_,_) -> (T_Int, [])
                    | _ -> error "Must be array"; raise Exit;
                end
            | _ -> error "Must be array"; raise Exit;
      end
  | E_Ifthenelse (exp1, exp2, exp3)  -> (*Change ifthelse to ifthenelse*)
      let (ty1, cnstr1) = walk_expr exp1 in
      let (ty2, cnstr2) = walk_expr exp2 in
      let (ty3, cnstr3) = walk_expr exp3 in
        ty2, (ty1, T_Bool) :: (ty2, ty3) :: cnstr1 @ cnstr2 @ cnstr3
  | E_Ifthen (exp1, exp2)  -> 
      let (ty1, cnstr1) = walk_expr exp1 in
      let (ty2, cnstr2) = walk_expr exp2 in
        T_Unit, (ty1, T_Bool) :: (ty2, T_Unit) :: cnstr1 @ cnstr2
  | E_Id (id, l)      -> 
      begin
        let id_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
          match id_entry.entry_info with
            | ENTRY_function func -> 
                let rec walk_params_list param func_param =
                  match param, func_param  with
                    | [], []  -> (func.function_result, [])
                    | x :: xs, [] -> error "Too many arguments"; raise Exit;
                    | [], y :: ys -> 
                        let tyy = match y.entry_info with
                          | ENTRY_parameter par_info -> par_info.parameter_type
                          | _ -> internal "Not a parameter"; 
                        in
                        let typ, constr = walk_params_list [] ys in
                          (T_Arrow (tyy,typ), constr)
                    | x :: xs, y :: ys -> 
                        let tyx, constrx = walk_atom x in
                        let tyy = match y.entry_info with
                          | ENTRY_parameter par_info -> par_info.parameter_type
                          | _ -> internal "Not a parameter";
                        in 
                        let typ, cnstr = walk_params_list xs ys in
                          (typ, (tyx,tyy) :: constrx @ cnstr)
                in
                  walk_params_list l (func.function_paramlist) 
            | _ -> error "This expretion %s is not a function" id; raise Exit;
      end
  | E_Cid (id, l)     -> 
      let cid_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
          | ENTRY_constructor constructor_info ->
              let constraints = 
                try (List.fold_left2 (fun acc atom typ ->
                                        let (atom_typ, atom_constr) = walk_atom atom in
                                          (atom_typ, typ) :: atom_constr @ acc ) [] l (constructor_info.constructor_paramlist)) 
                with Invalid_argument _ -> error "invalid number of arguments\n"; raise Exit
              in
                (constructor_info.constructor_type, constraints)
          | _ -> internal "Kaname malakia, expected constructor"
        end
  | E_Match (expr, l)    -> 
      begin 
        let (expr_typ, expr_constr) = walk_expr expr in
          let (result_typ, pat_typ, result_constr) = walk_clause_list l in
            (result_typ, (expr_typ, pat_typ) :: expr_constr @ result_constr)
      end
  | E_New ty1         ->
      (T_Ref ty1, [])           (*Must not be array - need to do that*)
  | E_Letin (l, e)    -> 
        openScope();
        walk_stmt l;
        let (typ, cnstr) = walk_expr e in
          closeScope();
          (typ, cnstr)       
  | E_Atom a          -> walk_atom a

and walk_expr_list t = match t with
  | []                -> ()
  | h::t              -> 
      let (constr,typ) = walk_expr h in
        walk_expr_list t


and walk_atom t = match t with 
  | A_Num n           -> T_Int, []
  | A_Dec f           -> T_Float, []
  | A_Chr c           -> T_Char,  []
  | A_Str str         -> T_Array(T_Char,1), []
  | A_Bool b          -> T_Bool, []
  | A_Cid cid         -> 
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
            | ENTRY_constructor constructor_info -> (constructor_info.constructor_type, [])
            | _ -> internal "internal error"
        end
  | A_Var v           -> 
      begin
        let s1 = lookupEntry (id_make v) LOOKUP_ALL_SCOPES true in 
          match s1.entry_info with
            | ENTRY_none | ENTRY_temporary _ | ENTRY_udt | ENTRY_constructor _ -> internal "Must be a variable, param or function";
            | ENTRY_variable var -> var.variable_type, []
            | ENTRY_function f ->
                let rec aux param_list =
                  match param_list with
                    | [] -> f.function_result
                    | (x::xs) ->
                        let tyx = match x.entry_info with
                          | ENTRY_parameter par_info -> par_info.parameter_type
                          | _ -> internal "Must be variable";
                        in
                          T_Arrow(tyx,aux xs)
                in
                  ((aux f.function_paramlist), [])
            | ENTRY_parameter par -> par.parameter_type, []
      end
  | A_Par             -> T_Unit, []
  | A_Bank a          -> 
      begin 
        let tya, constra = walk_atom a in
          match tya with 
            | T_Ref ty -> (ty, constra)
            | _ -> error "Must be a reference\n"; raise Exit;
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
                              | [],_ | _, 0 -> error "array dimensions are %d\n" dim; raise Exit;
                              | (x::xs), n -> 
                                  let tyx, constrx = walk_expr x in 
                                    walk_array_expr xs (n-1) ((tyx, T_Int) :: constrx @ acc)
                          in
                            T_Ref(typ), walk_array_expr b dim []
                      | _ -> error "must be an array\n"; raise Exit; (*What's wrong with pretty print..*)
                  end
            | _ -> error "must be an array\n"; raise Exit;
        end
  | A_Expr a          -> walk_expr a



(*acc = [(tp1,te1,constr1),(tp2,te2,constr2),(tp3,te3,constr3)...]
 * (tp1,te) :: [(tp1,tp2),(tp2,tp3),(tp3,...),(te1,te2),(te2,te3),(te3,..)]@constr1@constr2...@constre
 *)
and walk_clause_list lst =   (* Returns ( result_type , pattern_type, constraints)*)
  let rec walk_clause_aux l prev_clause acc =
      let (prev_pat_typ, prev_expr_typ, prev_constr) = prev_clause in
      match l with
        | []     -> (prev_expr_typ, prev_pat_typ , prev_constr @ acc)  
        | h :: t   -> 
            let (pat_typ, expr_typ, constr) = walk_clause h in
              walk_clause_aux t (pat_typ, expr_typ, constr) ((pat_typ, prev_pat_typ) :: (expr_typ, prev_expr_typ) :: acc @ prev_constr) 
  in
    match lst with 
      | []   -> internal "Clause list cannot be empty"
      | h :: t -> walk_clause_aux t (walk_clause h) []

and walk_clause t = match t with 
  | Clause(p,e)         -> 
      openScope(); (* should consider opening only when needed *)
      let (pat_type, pat_constraints) = walk_pattern p in
      let (expr_type, expr_constr) = walk_expr e in 
        closeScope();
        (pat_type, expr_type, expr_constr @ pat_constraints)

and walk_pattern p = match p with
  | Pa_Atom a         -> walk_pattom a
  | Pa_Cid (cid, l)   ->  
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        match cid_entry.entry_info with
          | ENTRY_constructor constructor_info -> 
              let constraints = 
                try ( List.fold_left2 (fun acc pattom typ -> 
                                       let (pattom_typ, patom_constraints) = walk_pattom pattom in
                                         (pattom_typ, typ) :: patom_constraints @ acc) [] l constructor_info.constructor_paramlist )
                with Invalid_argument _ -> error "Wrong number of constructor arguments\n"; raise Exit
              in
                (constructor_info.constructor_type, constraints)
          | _ -> internal "we failed you again and again"

and walk_pattom t = match t with
  | P_Sign(op, num)   ->
      begin
        match op with 
          | P_Plus        -> (T_Int, [])
          | P_Minus       -> (T_Int, [])
      end
  | P_Fsign(op, num)  ->
      begin
        match op with 
          | P_Fplus       -> (T_Float, [])
          | P_Fminus      -> (T_Float, [])
      end
  | P_Num n           -> (T_Int, [])
  | P_Float f         -> (T_Float, [])
  | P_Chr c           -> (T_Char, [])
  | P_Bool b          -> (T_Bool, [])
  | P_Id id           -> 
      let new_ty = fresh() in
      let s1 = newVariable (id_make id) new_ty true in
        ignore s1;
        (new_ty, [])
  | P_Cid cid         -> 
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
            | ENTRY_constructor constructor_info -> (constructor_info.constructor_type, [])
            | _ -> internal "we failed you again"
        end
  | P_Pattern p       -> walk_pattern p
  

