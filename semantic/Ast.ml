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
  let constraints = walk_stmt_list ls in
  unify constraints

and walk_stmt_list ls = 
  let walk_stmt_list_aux ls acc = match ls with
    | []                  ->  acc
    | h :: t              ->
        printState "Before opening" "After opening" (openScope) ();
        let constraints = walk_stmt h in
          walk_stmt_list_aux t (constraints :: acc);
          printState "Before closing" "Afterclosing" (closeScope) ();
  in
    walk_stmt_list_aux ls []

and walk_stmt t = match t with
  | S_Let  l          -> walk_def_list l
  | S_Rec l           -> 
      List.iter walk_recdef_names l;      
      let constraints = walk_recdef_list l in
        constraints
  | S_Type l          -> walk_typedef_list l

and walk_def_list t = match t with
  | []                  -> []
  | h :: t              ->  
      let constraints = walk_def_list t in
      (walk_def h) @ constraints

and walk_recdef_list t  = match t with
  | []                -> []
  | h :: t            ->
      walk_recdef_params h;
      let constraints = walk_recdef_list t in
      (walk_recdef h) @ constraints  

and walk_def t = match t.def with
  | D_Var (l, e)      ->
      begin
        match l with
          | []            -> internal "Definition cannot be empty";
          | (id, ty) :: []  ->
                let new_ty = refresh ty in
                (*printState "Before hiding" "After hiding" (hideScope !currentScope) (true); --probably not needed *)
                let constraints = walk_expr e in
                let p = newVariable (id_make id) new_ty true in
                  ignore p;
                  ((new_ty, e.expr_typ) :: constraints)
                  (* printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false); --probably not needed *)

          | (id, ty) :: tl  ->
              let p = newFunction (id_make id) true in 
                (* printState "Before opening" "After opening" (openScope()); *)
                printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
                printState "Before opening" "After opening" (openScope) ();
                walk_par_list tl p;
                let new_ty = refresh ty in
                endFunctionHeader p new_ty;
                (* printState "Before opening" "After opening" (openScope) (); *)
                (* show_par_to_expr tl; *)
                let constraints = walk_expr e in 
                  printState "Before closing" "Afterclosing" (closeScope) ();
                  printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
                  ((new_ty, e.expr_typ) :: constraints)
      end
  | D_Mut (id, ty) ->     
      let new_ty = refresh ty in
      let p = newVariable (id_make id) (T_Ref new_ty) true in
        ignore p;
        []
  | D_Array (id, ty, l) ->
      let rec walk_expr_list l acc = match l with
        | [] -> acc
        | e :: t -> 
            let constraints = walk_expr e in
              walk_expr_list t (((e.expr_typ, T_Int) :: constraints) @ acc)
      in
      let new_ty = refresh ty in
      let p = newVariable (id_make id) (T_Array (new_ty, List.length l)) true in
        ignore p;
        printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
        let constraints = walk_expr_list l in
        printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
        constraints
        
and walk_recdef_names t = match t.def with
  | D_Var (l, e)       ->
      begin 
        match l with
          | [] -> internal "Definition cannot be empty";
          | (id, ty) :: []  ->
              let new_ty = refresh ty in
              let p = newVariable (id_make id) new_ty true in
                ignore p;
          | (id, ty) :: tl  -> 
              let new_ty = refresh ty in
              let p = newFunction (id_make id) true in
                setType p new_ty;
                forwardFunction p
      end
  | D_Mut (id, t)       -> error "Mutable cannot be rec\n";
  | D_Array (id, t, l)  -> error "Array cannot be rec\n";

and walk_recdef_params t = match t.def with
  | D_Var(l, e)  ->
    begin 
        match l with
          | [] -> internal "too many problems\n"; 
          | (id, ty) :: []  -> ()
          | (id, ty) :: tl  -> 
              let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
              let new_ty = getType p in
              printState "Before opening" "After opening" (openScope) ();
              walk_par_list tl p;
              printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
              endFunctionHeader p new_ty;
      end
  | D_Mut (id, t)       -> error "Mutable cannot be rec\n";
  | D_Array (id, t, l)  -> error "Array cannot be rec\n";   

and walk_recdef t = match t.def with
  | D_Var (l, e)      -> 
      begin 
        match l with
          | []            -> error "too many problems\n"; raise Exit
          | (id, ty) :: []  -> 
              printState "Before hiding" "After hiding" (hideScope !currentScope) (true);
              let constraints = walk_expr e in 
              printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
              let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
              let new_ty = getType p in
                (new_ty, e.expr_typ) :: constraints
          | (id, ty) :: tl  -> 
              (* let p = newFunction (id_make id) true in *) 
              (*   printState "Before opening" "After opening" (openScope) (); *)
              (*   walk_par_list tl p; *)
              (*   endFunctionHeader p new_ty; *)
                let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
                let new_ty = getType p in
                printState "Before unhiding" "After unhiding" (hideScope !currentScope) (false);
                let constraints = walk_expr e in
                  printState "Before closing" "Afterclosing" (closeScope) ();
                 (new_ty, e.expr_typ) :: constr
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

and walk_expr expr_node = match expr_node.expr with 
  | E_Binop (expr1, op, expr2) -> 
      begin 
        match op with 
          | Plus | Minus | Times | Div | Mod  -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
                expr_node.expr_typ <- T_Int;
                (expr1.expr_typ, T_Int) :: (expr2.expr_typ, T_Int) :: constraints1 @ constraints2
          | Fplus | Fminus | Ftimes | Fdiv | Power  -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
                expr_node.expr_typ <- T_Float; 
                (expr1.expr_typ, T_Float) :: (expr2.expr_typ, T_Float) :: constraints1 @ constraints2
          | Seq | Nseq | Eq | Neq       -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
              expr_node.expr_typ <- T_Bool; 
              (expr1.expr_typ, expr2.expr_typ ) :: constraints1 @ constraints2 (*Must not be array or function - need to do that*)
          | L | Le | G  | Ge  -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
                expr_node.expr_typ <- T_Bool; 
                (expr1.expr_typ,T_Int) :: (expr1.expr_typ, expr2.expr_typ) :: constraints1 @ constraints2 (*ty1 = T_Int | T_Float | T_Char, using T_Int for the moment, type typ_inf = Ord | Typ of typ or [typ] for all*)
          | And | Or      -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
                expr_node.expr_typ <- T_Bool; 
                (expr1.expr_typ, T_Bool) :: (expr2.expr_typ, T_Bool) :: constraints1 @ constraints2
          | Semicolon     -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in
                 expr_node.expr_typ <- expr2.expr_typ;
                 constraints1 @ constraints2
          | Assign        -> 
              let constraints1 = walk_expr expr1 in
              let constraints2 = walk_expr expr2 in     
                expr_node.expr_typ <- T_Unit;   
                match (expr1.expr_typ) with 
                  | T_Ref typ -> (typ, expr2.expr_typ) :: constraints1 @ constraints2
                  | typ -> 
                    let (line, char_pos) = expr1.pos in 
                      error "Line: %d Character: %d -> This expression has type %a but an expression was expected of type %a" line char_pos pretty_typ typ pretty_typ (T_Ref expr2.expr_typ) (*ain't gonna play, but keep it as a sample*)
                      raise Exit;
      end
  | E_Unop (op, expr1)     ->
      begin
        match op with
          | U_Plus | U_Minus -> 
              let constraints1 = walk_expr expr1 in 
                expr_node.expr_typ <- T_Int;
                (expr1.expr_typ, T_Int) :: constraints1
          | U_Fplus | U_Fminus  -> 
              let constraints1 = walk_expr expr1 in 
                expr_node.expr_typ <- T_Float;
                T_Float, (ty1, T_Float) :: constraints1
          | U_Del         -> 
              let constraints1 = walk_expr expr1 in
                expr_node.expr_typ <- T_Unit;
                match (expr1.expr_typ) with 
                  | T_Ref _ -> constraints1
                  | typ -> 
                    let (line, char_pos) = expr1.pos in 
                      error "Line: %d Character: %d -> This expression has type %a but an expression was expected of type 'a ref" line char_pos pretty_typ typ pretty_typ; (*ain't gonna play, but keep it as a sample*)
                      raise Exit;
          | U_Not         -> 
              let constraints1 = walk_expr expr1 in
                expr_node.expr_typ <- T_Bool;
                (expr1.expr_typ, T_Bool) :: constraints1
      end
  | E_Block expr1    -> walk_expr expr1
  | E_While (expr1, expr2)    -> 
     let constraints1 = walk_expr expr1 in
     let constraints2 = walk_expr expr2 in  
        expr_node.expr_typ <- T_Unit;
        (expr1.expr_typ, T_Bool) :: (expr2.expr_typ, T_Unit) :: constraints1 @ constraints2
  | E_For (id, expr1, cnt, expr2, expr3) ->
      openScope();
      let i = newVariable (id_make id) T_Int true in
        ignore i ;
        let constraints1 = walk_expr expr1 in
        let constraints2 = walk_expr expr2 in 
        let constraints3 = walk_expr expr3 in
          closeScope();
          expr_node.entry <- i;
          expr_node.expr_typ <- T_Unit;
          (expr1.expr_typ, T_Int) :: (expr2.expr_typ, T_Int) :: (expr3.expr_typ, T_Unit) :: constraints1 @ constraints2 @ constraints3                        
  | E_Dim (a, id)      ->
      begin
        let id_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
          match id_entry.entry_info with
            | ENTRY_variable var -> 
                begin
                  match var.variable_type with
                    | T_Array (_,_) ->
                      expr_node.expr_typ <- T_Int;
                      expr_node.entry <- id_entry;
                      []
                    | _ -> error "Must be array"; raise Exit;
                end
            | _ -> error "Must be array"; raise Exit;
      end
  | E_Ifthenelse (expr1, expr2, expr3)  -> (*Change ifthelse to ifthenelse*)
        let constraints1 = walk_expr expr1 in
        let constraints2 = walk_expr expr2 in 
        let constraints3 = walk_expr expr3 in
        expr_node.expr_typ <- expr2.expr_typ;
        (expr1.expr_typ, T_Bool) :: (expr2.expr_typ, expr3.expr_typ) :: constraints1 @ constraints2 @ constraints3
  | E_Ifthen (expr1, expr2)  -> 
        let constraints1 = walk_expr expr1 in
        let constraints2 = walk_expr expr2 in 
        expr_node.expr_typ <- T_Unit;
        (expr1.expr_typ, T_Bool) :: (expr2.expr_typ, T_Unit) :: constraints1 @ constraints2
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
                          (typ, (tyx, tyy) :: constrx @ cnstr)
                in
                  let (typ, constraints) = walk_params_list l (func.function_paramlist) in
                    expr_node.expr_typ <- typ;
                    expr_node.entry <- id_entry;
                    constraints
            | _ -> error "This expression %s is not a function" id; raise Exit;
      end
  | E_Cid (id, l)     -> 
      let cid_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
          | ENTRY_constructor constructor_info ->
              let constraints = 
                try ( List.fold_left2 (fun acc atom typ ->
                                        let (atom_typ, atom_constr) = walk_atom atom in
                                          (atom_typ, typ) :: atom_constr @ acc ) [] l (constructor_info.constructor_paramlist)) 
                with Invalid_argument _ -> error "invalid number of arguments\n"; raise Exit
              in
                expr_node.expr_typ <- constructor_info.constructor_type;
                expr_node.entry <- cid_entry;
                constraints
          | _ -> internal "Kaname malakia, soz. expected constructor gia na ksereis"
        end
  | E_Match (expr1, l)    -> 
      begin 
        let constraints1 = walk_expr expr1 in
          let (result_typ, pat_typ, result_constraints) = walk_clause_list l in
            expr_node.expr_typ <- result_typ;
            (expr1.expr_typ, pat_typ) :: constraints1 @ result_constraints
      end
  | E_New ty1         ->
      match ty1 with
        | T_Array _, _ -> error "Cannot dynamically allocate array. This is not ruby. Or python. Or C. THIS IS LLAMA!"; raise Exit;
        | ty1 -> 
          expr_node.expr_typ <- ty1;
          []
  | E_Letin (l, expr)    -> 
        openScope();
        let constraints1 = walk_stmt l in
        let constraints2 = walk_expr expr in
          expr_node.expr_typ <- expr.expr_typ;
          closeScope();
          constraints1 @ constraints2       
  | E_Atom a          -> 
    let constrains = walk_atom a in
    expr_node.expr_typ <- a.atom_typ;
    constraints

and walk_atom t = match t.atom with 
  | A_Num n           -> t.atom_typ <- T_Int; []
  | A_Dec f           -> t.atom_typ <- T_Float; []
  | A_Chr c           -> t.atom_typ <- T_Char;  []
  | A_Str str         -> t.atom_typ <- T_Array(T_Char, 1); []
  | A_Bool b          -> t.atom_typ <- T_Bool; []
  | A_Cid cid         -> 
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
            | ENTRY_constructor constructor_info -> 
              t.atom_typ <- constructor_info.constructor_type; 
              t.entry <- cid_entry;
              []
            | _ -> internal "internal error"
        end
  | A_Var v           -> 
      begin
        let id_entry = lookupEntry (id_make v) LOOKUP_ALL_SCOPES true in 
          match id_entry.entry_info with
            | ENTRY_none | ENTRY_temporary _ | ENTRY_udt | ENTRY_constructor _ | ENTRY_function _-> internal "Must be a variable, param";
            | ENTRY_variable var -> 
              t.atom_typ <- var.variable_type; 
              t.entry <- id_entry;
              []
            (*| ENTRY_function f ->   Opws ola ta wraia pragmata apagoreuetai h merikh efarmogh.
                let rec aux param_list =
                  match param_list with
                    | [] -> f.function_result
                    | x :: xs ->
                        let tyx = match x.entry_info with
                          | ENTRY_parameter par_info -> par_info.parameter_type
                          | _ -> internal "Must be variable";
                        in
                          T_Arrow(tyx, aux xs)
                in
                  ((aux f.function_paramlist), [])*)
            | ENTRY_parameter par -> 
              t.atom_typ <- par.parameter_type; 
              t.entry <- id_entry;
              []
      end
  | A_Par             -> t.atom_typ <- T_Unit; []
  | A_Bank atom          -> 
      begin 
        let constraints = walk_atom atom in
          match atom.atom_typ with 
            | T_Ref typ -> t.atom_typ <- typ; constraints
            | _ -> error "Must be a reference\n"; raise Exit;
      end
  | A_Array (id, expr_list)    -> 
      let array_entry = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        begin
          match array_entry.entry_info with 
            | ENTRY_variable arr -> 
                let typ_arr = arr.variable_type in
                  begin
                    match typ_arr with 
                      | T_Array(typ, dim) -> 
                          let rec walk_array_expr expr_list n acc =
                            match expr_list, n with
                              | [], 0 -> acc
                              | [], _ | _, 0 -> error "array dimensions are %d\n" dim; raise Exit;
                              | (expr :: xs), n -> 
                                  let constraints = walk_expr expr in 
                                    walk_array_expr xs (n-1) ((expr.expr_typ, T_Int) :: constraints @ acc)
                          in
                            t.atom_typ <- T_Ref typ;
                            t.entry <- array_entry;
                            walk_array_expr expr_list dim []
                      | _ -> error "must be an array\n"; raise Exit; (*What's wrong with pretty print..*)
                  end
            | _ -> error "must be an array\n"; raise Exit;
        end
  | A_Expr expr          -> 
    let constraints = walk_expr expr in
      t.atom_typ <- expr.expr_typ;
      constraints


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
  | Clause(pat, expr)         -> 
      openScope(); (* should consider opening only when needed *)
      let pat_constrains = walk_pattern pat in
      let expr_constraints = walk_expr expr in 
        closeScope();
        (pat.pattern_typ, expr.expr_typ, expr_constraints @ pat_constraints)

and walk_pattern p = match p.pattern with
  | Pa_Atom a         -> 
      let constraints = walk_pattom a in
        p.pattern_typ <- a.pattom_typ;
        constraints
  | Pa_Cid (cid, l)   ->  
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        match cid_entry.entry_info with
          | ENTRY_constructor constructor_info -> 
              let constraints = 
                try ( List.fold_left2 (fun acc pattom typ -> 
                                         let patom_constraints = walk_pattom pattom in
                                         (pattom.pattom_typ, typ) :: patom_constraints @ acc ) [] l constructor_info.constructor_paramlist )
                with Invalid_argument _ -> error "Wrong number of constructor arguments\n"; raise Exit
              in
                p.pattern_typ <- constructor_info.constructor_type;
                p.entry <- cid_entry;
                constraints
          | _ -> internal "we failed you again and again"

and walk_pattom t = match t.pattom with
  | P_Sign(op, num)   ->
      begin
        match op with 
          | P_Plus        -> t.pattom_typ <- T_Int; []
          | P_Minus       -> t.pattom_typ <- T_Int; []
      end
  | P_Fsign(op, num)  ->
      begin
        match op with 
          | P_Fplus       -> t.pattom_typ <- T_Float; []
          | P_Fminus      -> t.pattom_typ <- T_Float; []
      end
  | P_Num n           -> t.pattom_typ <- T_Int;   []
  | P_Float f         -> t.pattom_typ <- T_Float; []
  | P_Chr c           -> t.pattom_typ <- T_Char;  []
  | P_Bool b          -> t.pattom_typ <- T_Bool;  []
  | P_Id id           -> 
      let new_ty = fresh() in
      let id_entry = newVariable (id_make id) new_ty true in
        ignore id_entry;
        t.pattom_typ <- new_ty;
        t.entry <- id_entry;
        []
  | P_Cid cid         -> 
      let cid_entry = lookupEntry (id_make cid) LOOKUP_ALL_SCOPES true in
        begin
          match cid_entry.entry_info with
            | ENTRY_constructor constructor_info -> 
                t.pattom_typ <- constructor_info.constructor_type;
                t.entry <- cid_entry;
                []
            | _ -> internal "we failed you again"
        end
  | P_Pattern p       -> 
      let constraints = walk_pattern (P_Pattern p) in
        t.pattom_typ <- p.pattern_typ;
        constraints
  | _ -> internal "An error occured. Contact your system administrator muahahaha"
