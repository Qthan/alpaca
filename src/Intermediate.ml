open Identifier
open Types
open AstTypes
open SymbTypes
open Quads
open Error

exception InvalidCompare of typ

let debug_quads = false 

(*  Symbol entry option to unsolved type *)
let lookup_type entry =
  match entry with
    | None -> internal "Entry not found\n"
    | Some e -> Symbol.getType e

let lookup_res_type entry =
  match entry with
    | None -> internal "Entry not found\n"
    | Some e -> Symbol.getResType e

let update_entry_typ entry =
  let fresh_typ = Symbol.getResType entry in
  let typ = Typeinf.lookup_solved fresh_typ in
    Symbol.setType entry typ

let update_def_typ def = 
  match def.def_entry with 
    | None -> internal "Definition must have an entry"
    | Some e -> 
      update_entry_typ e;
      match e.entry_info with
        | ENTRY_function f -> List.iter update_entry_typ f.function_paramlist
        | _ -> ()

(* Check if definition is function *)
let is_fun def =
  match def.def with
    | D_Var (lst, _) -> 
      (match lst with
        | [_] -> false
        | _ -> true)
    | _  ->  false

let split_decls decl = 
  match decl with 
    | S_Let lst -> 
      let (funs, vars) = List.partition (is_fun) lst in
        (S_Let funs, S_Let vars)
    | S_Rec lst ->
      let (funs, vars) = List.partition (is_fun) lst in
        (S_Rec funs, S_Rec vars)
    | S_Type _ -> internal "Cannot have type defs in such position"

(* XXX Check if type is unit XXX*)
let isUnit fresh_typ = 
  match (Typeinf.lookup_solved fresh_typ) with 
    | T_Unit -> true
    | _ -> false

let fun_stack = Stack.create ()

let isTail expr = 
  match expr.expr_entry with
      None -> internal "Function entry not found"
    | Some e ->
      let cur_fun = Stack.top fun_stack in
        !Quads.tailRecOpt && expr.expr_tail && (Symbol.entry_eq cur_fun e)

let rec gen_program ast subst outer_entry =
  let quads = gen_decl_list ast outer_entry in
  let finalQuads = normalizeQuads (List.rev quads) in
    if (debug_quads) then printQuads Format.std_formatter finalQuads;
    finalQuads

and gen_decl_list ast outer_entry = 
  let delete_quads = ref (newQuadList ()) in
  let () = Symbol.fixOffsets outer_entry in
  let () = Stack.push outer_entry fun_stack in
  let outer = 
    genQuad (Q_Unit, O_Entry outer_entry, O_Empty, O_Empty) (newQuadList ()) 
  in
  let quads = List.fold_left (fun o a -> gen_decl o a delete_quads) outer ast in
  let quads2 = mergeQuads !delete_quads quads in
  let final = genQuad (Q_Endu, O_Entry outer_entry, O_Empty, O_Empty) quads2 in
    final

and gen_decl outer stmt delete_quads = match stmt with
  | S_Let l | S_Rec l -> 
    List.iter update_def_typ l;
    gen_def_list outer l delete_quads
  | S_Type l -> 
    let fquads = List.fold_left (fun quads (ty_name, _) -> 
        gen_type_eq ty_name quads) (newQuadList ()) l in
      mergeQuads outer fquads 

(* Generates equality function for a UDT *)
and gen_type_eq ty_name quads =
  let u_entry = Symbol.lookupUdt (id_make ty_name) in
  let constructors = Symbol.getConstructors u_entry in
  let eqfun_entry = Symbol.getEqFun u_entry in
  let eqfun_inf = match eqfun_entry.entry_info with
    | ENTRY_function f -> f
    | _ -> internal "Not a function"
  in
  let () = Stack.push eqfun_entry fun_stack in
  let (a, b) = match eqfun_inf.function_paramlist with
    | [a; b] -> (a, b)
    | _ -> internal "wrong number of arguments - our fault"
  in
  let temp_a = newTemp (T_Ref T_Int) (Stack.top fun_stack) false in
  let temp_b = newTemp (T_Ref T_Int) (Stack.top fun_stack) false in
  (* Comparing left-most constructor *)
  let quads1 = genQuad (Q_Unit, O_Entry eqfun_entry, O_Empty, O_Empty) quads in
  let quads2 = genQuad (Q_Constr, O_Entry a, O_Int 0, temp_a) quads1 in
  let quads3 = genQuad (Q_Constr, O_Entry b, O_Int 0, temp_b) quads2 in
  let tag_t = Labels.makeLabelList (Label.nextLabel ()) in
  let quads4 = 
    genQuad (Q_Seq, O_Deref temp_a, O_Deref temp_b, O_Backpatch) quads3 in
  let tag_f = Labels.makeLabelList (Label.nextLabel ()) in
  let quads5 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads4 in
  let quads6 = backpatch quads5 tag_t (Label.nextLabel ()) in
  (* If left-most constructors match then depending on the constructor compare
   * the constructor's arguments *)
  let (quads7, cond_info) =
    (* Constructor iteration *)
    List.fold_left (fun (quads, cond_info) c_entry ->
        let c_tag = Symbol.getTag c_entry in
        let c_args = Symbol.getConstructorParamList c_entry in
        let isCnstr_t = Labels.makeLabelList (Label.nextLabel ()) in
        let quads1 =
          genQuad (Q_Seq, O_Deref temp_a, O_Int c_tag, O_Backpatch) quads in
        let isCnstr_f = Labels.makeLabelList (Label.nextLabel ()) in
        let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
        let quads3 = backpatch quads2 isCnstr_t (Label.nextLabel ()) in
        let (quads4, _, res_f) =
          (* Argument iteration *)
          List.fold_left (fun (quads, offset, false_lst) arg_typ ->
              let arg_a = newTemp (T_Ref arg_typ) (Stack.top fun_stack) false in
              let arg_b = newTemp (T_Ref arg_typ) (Stack.top fun_stack) false in
              let quads1 =
                genQuad (Q_Constr, O_Entry a, O_Int offset, arg_a) quads in
              let quads2 =
                genQuad (Q_Constr, O_Entry b, O_Int offset, arg_b) quads1 in
                match arg_typ with
                  | T_Id id ->
                    let result = newTemp (T_Bool) (Stack.top fun_stack) false in
                    let argtyp_entry = Symbol.lookupUdt (id_make id) in
                    let eqfun_entry = Symbol.getEqFun argtyp_entry in
                    (* Recursive equality check for arguments *)
                    let quads3 = 
                      genQuad (Q_Par, O_Deref arg_a, O_ByVal, O_Empty) quads2 in
                    let quads4 = 
                      genQuad (Q_Par, O_Deref arg_b, O_ByVal, O_Empty) quads3 in
                    let quads5 =
                      genQuad (Q_Par, result, O_Ret, O_Empty) quads4 in
                    let quads6 =
                      genQuad (Q_Call, O_Empty, O_Empty, O_Entry eqfun_entry)
                        quads5 in
                    let argeq_t = Labels.makeLabelList (Label.nextLabel ()) in
                    let quads7 =
                      genQuad (Q_Ifb, result, O_Empty, O_Backpatch) quads6 in
                    let argeq_f = Labels.makeLabelList (Label.nextLabel ()) in
                    let quads8 =
                      genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads7 in
                    let quads9 = 
                      backpatch quads8 argeq_t (Label.nextLabel ()) 
                    in
                    let false_lst = Labels.mergeLabels false_lst argeq_f in
                      (quads9, offset + (Types.sizeOfType arg_typ), false_lst)
                  | typ ->
                    let argeq_t = Labels.makeLabelList (Label.nextLabel ()) in
                    let quads3 =
                      genQuad (Q_Seq, O_Deref arg_a, O_Deref arg_b, O_Backpatch)
                        quads2 in
                    let argeq_f = Labels.makeLabelList (Label.nextLabel ()) in
                    let quads4 =
                      genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
                    let quads5 = 
                      backpatch quads4 argeq_t (Label.nextLabel ()) 
                    in
                    let false_lst = Labels.mergeLabels false_lst argeq_f in
                      (quads5, offset + (Types.sizeOfType arg_typ), false_lst)
            ) (quads3, Types.tag_size, Labels.newLabelList ()) c_args
        in
        let res_t = Labels.makeLabelList (Label.nextLabel ()) in
        (* Jump to successful equality check *)
        let quads5 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads4 in
        let quads6 = backpatch quads5 isCnstr_f (Label.nextLabel ()) in
        let new_t = Labels.mergeLabels res_t (cond_info.true_lst) in
        let false_t = Labels.mergeLabels res_f (cond_info.false_lst) in
          (quads6, setCondInfo new_t false_t)
      ) (quads6, setCondInfo (Labels.newLabelList ()) tag_f) constructors
  in
  let err_str = 
    "Reached unreachable point in data type equality. Internal error\n"
  in 
  let quads8 = genQuad (Q_Par, O_Str err_str, O_ByVal, O_Empty) quads7 in
  let quads9 = 
    genQuad (Q_Call, O_Empty, O_Empty, O_Entry !Ast.print_string_entry) quads8 
  in 
  let quads10 = genQuad (Q_Fail, O_Empty, O_Empty, O_Empty) quads9 in
  let quads11 = backpatch quads10  (cond_info.true_lst) (Label.nextLabel ()) in
  let quads12 = genQuad (Q_Assign, O_Bool true, O_Empty, O_Res) quads11 in
  let the_end = Labels.makeLabelList (Label.nextLabel ()) in
  let quads13 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads12 in
  let quads14 = backpatch quads13 cond_info.false_lst (Label.nextLabel ()) in
  let quads15 = genQuad (Q_Assign, O_Bool false, O_Empty, O_Res) quads14 in
  let quads16 = backpatch quads15 the_end (Label.nextLabel ()) in
  let quads17 =
    genQuad (Q_Endu, O_Entry eqfun_entry, O_Empty, O_Empty) quads16 in
  let _ = Stack.pop fun_stack in
    quads17

and gen_def_list outer lst delete_quads =
  List.fold_left (fun o l -> gen_def o l delete_quads ) outer lst

and gen_def quads def_node delete_quads =  
  let entry = match def_node.def_entry with 
    | Some e -> e
    | None -> internal "No entry for function"
  in
    match def_node.def with
      | D_Var (lst, expr) -> 
        begin 
          match lst with
            | [] -> internal "Definition cannot be empty."
            | (id, _) :: []     ->
              let typ = lookup_res_type (Some entry) in
                if (isUnit typ)
                then
                  let (quads1, s_info) = gen_stmt quads expr in
                  let quads2 = 
                    backpatch quads1 s_info.next_stmt (Label.nextLabel ()) 
                  in
                    quads2
                else
                  let (quads1, e_info) = gen_expr quads expr in
                  let quads2 = 
                    backpatch quads1 e_info.next_expr (Label.nextLabel ()) 
                  in
                  let quads3 = 
                    genQuad (Q_Assign, e_info.place, O_Empty, O_Entry entry) 
                      quads2 
                  in
                    quads3
            | (id, _) :: params ->
              let () = Symbol.fixOffsets entry in
              let typ = lookup_res_type (Some entry) in
              let fQuads = newQuadList () in
              let () = Stack.push entry fun_stack in
              let fQuads1 = 
                genQuad (Q_Unit, O_Entry entry, O_Empty, O_Empty) fQuads 
              in
              let () = Symbol.setFunctionLabel entry (Label.nextLabel ()) in
                if (isUnit typ)
                then
                  let (fQuads2, s_info) = gen_stmt fQuads1 expr in
                  let fQuads3 = 
                    backpatch fQuads2 s_info.next_stmt (Label.nextLabel ()) 
                  in
                  let fQuads4 = 
                    genQuad (Q_Endu, O_Entry entry, O_Empty, O_Empty) fQuads3 
                  in 
                  let _ = Stack.pop fun_stack in
                    mergeQuads quads fQuads4 
                else
                  let (fQuads2, e_info) = gen_expr fQuads1 expr in
                  let fQuads3 = 
                    backpatch fQuads2 e_info.next_expr (Label.nextLabel ()) 
                  in
                  let fQuads4 = 
                    genQuad (Q_Assign, e_info.place, O_Empty, O_Res) fQuads3
                  in
                  let fQuads5 = 
                    genQuad (Q_Endu, O_Entry entry, O_Empty, O_Empty) fQuads4 
                  in 
                  let _ = Stack.pop fun_stack in
                    mergeQuads quads fQuads5 
        end
      | D_Mut (id, _) -> 
        let typ = lookup_res_type (Some entry) in
        let size = sizeOfElement typ in
        let new_entry = Symbol.findAuxilEntry "_new" in
        let temp = newTemp (T_Ref typ) (Stack.top fun_stack) false in
        let quads1 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads in
        let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
        let quads3 = 
          genQuad (Q_Call, O_Empty, O_Empty, O_Entry new_entry) quads2 
        in
        let quads4 = genQuad (Q_Assign, temp, O_Empty , O_Entry entry) quads3 in
          quads4
      | D_Array (id, _, lst)  -> 
        let rec gen_array_dims lst quads =
          match lst with
            | [] -> quads  (* XXX: maybe internal here *)
            | dim :: xs ->
              let (quads1, e_info) = gen_expr quads dim in
              let quads2 = 
                genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads1 
              in
                gen_array_dims xs quads2
        in
        let quads1 = gen_array_dims lst quads in
        let ty = lookup_type def_node.def_entry in
        let size = sizeOfElement ty in  (* Size of an array element *)
        let quads2 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads1 in
        let makearr_entry = Symbol.findAuxilEntry "_make_array" in
        let delete_entry =  Symbol.findAuxilEntry "_delete_array" in
        let dims = (* --- Need to get an int out of Dim type --- *)
          match arrayDims ty with
            | D_Dim d -> d
            | D_DimSize _ 
            | D_Alpha _ -> internal "Unknown array dimensions\n"
        in
        let quads3 = genQuad (Q_Par, O_Int dims, O_ByVal, O_Empty) quads2 in
        let quads4 = genQuad (Q_Par, O_Entry entry , O_Ret, O_Empty) quads3 in 
        let quads5 = 
          genQuad (Q_Call, O_Empty, O_Empty, O_Entry makearr_entry) quads4 
        in
          delete_quads := genQuad (Q_Par, O_Entry entry, O_ByVal, O_Empty) 
              !delete_quads;
          delete_quads := genQuad 
              (Q_Call, O_Empty, O_Empty, O_Entry delete_entry) 
              !delete_quads;
          delete_quads := [];
          quads5

and gen_expr quads expr_node = match expr_node.expr with 
  | E_Binop (expr1, op, expr2) ->
    begin 
      match op with 
        | Plus | Minus | Times | Div | Mod
        | Fplus | Fminus | Ftimes | Fdiv as oper -> 
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 (e1_info.next_expr) (Label.nextLabel ()) 
          in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = 
            backpatch quads3 (e2_info.next_expr) (Label.nextLabel ()) 
          in
          let typ = expr_node.expr_typ in
          (* let solved = lookup_solved typ  in -- reduntant*) 
          let temp = newTemp typ (Stack.top fun_stack) false in
          let quads5 = 
            genQuad (getQuadBop oper, e1_info.place, e2_info.place, temp) quads4
          in
          let e_info = setExprInfo temp (Labels.newLabelList ()) in
            (quads5, e_info)
        | Power ->
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 (e1_info.next_expr) (Label.nextLabel ()) 
          in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = 
            backpatch quads3 (e2_info.next_expr) (Label.nextLabel ()) 
          in
          let quads5 = 
            genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads4 
          in
          let quads6 = 
            genQuad (Q_Par, e2_info.place, O_ByVal, O_Empty) quads5 
          in
          let temp = newTemp T_Float (Stack.top fun_stack) false in
          let quads7 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads6 in
          let pow_entry = Symbol.findAuxilEntry "_pow" in
          let quads8 = 
            genQuad (Q_Call, O_Empty, O_Empty, O_Entry pow_entry) quads7 
          in
          let e_info = setExprInfo temp (Labels.newLabelList ()) in
            (quads8, e_info)
        | Seq | Nseq | Eq | Neq
        | L | Le | G | Ge 
        | And | Or ->
          let (quads1, cond_info) = gen_cond quads expr_node in
          let quads2 = 
            backpatch quads1 cond_info.true_lst (Label.nextLabel ()) 
          in
          let temp = newTemp expr_node.expr_typ (Stack.top fun_stack) false in
          let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
          let e_info = 
            setExprInfo temp (Labels.makeLabelList (Label.nextLabel ())) 
          in
          let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let quads5 = 
            backpatch quads4 cond_info.false_lst (Label.nextLabel ()) 
          in
          let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
            (quads6, e_info)
        | Semicolon -> 
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = 
            backpatch quads1 stmt_info.next_stmt (Label.nextLabel ()) 
          in
          let (quads3, expr_info) = gen_expr quads2 expr2 in
            (quads3, expr_info)
        | Assign -> internal "Assign is not an expression\n"
    end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Plus | U_Minus | U_Fplus | U_Fminus as oper ->
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 e1_info.next_expr (Label.nextLabel ()) 
          in
          let typ = expr_node.expr_typ in
          let temp = newTemp typ (Stack.top fun_stack) false in
          let quads3 = match typ with
            | T_Int -> 
              genQuad (getQuadUnop oper, O_Int 0, e1_info.place, temp) quads2
            | T_Float -> 
              genQuad (getQuadUnop oper, O_Float 0., e1_info.place, temp) 
                quads2
            | _ -> internal "Arithmetic operations only work with Int/Floats"
          in
          let e_info = setExprInfo temp (Labels.newLabelList ()) in
            (quads3, e_info)
        | U_Del -> internal "Unreachable point: Delete is not an expression"
        (* Will create gen_stm and U_Del will go there*)
        (*let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads1 in
          let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Fun "_delete") in
          let e_info = 
          (quads3, e_info)*)
        | U_Not ->
          let (quads1, cond_info) = gen_cond quads expr_node in
          let quads2 = 
            backpatch quads1 cond_info.true_lst (Label.nextLabel ()) 
          in
          let temp = newTemp expr_node.expr_typ (Stack.top fun_stack) false in
          let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
          let e_info = 
            setExprInfo temp (Labels.makeLabelList (Label.nextLabel ())) 
          in
          let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let quads5 = 
            backpatch quads4 cond_info.false_lst (Label.nextLabel ()) 
          in
          let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
            (quads6, e_info)
    end
  | E_Id (id, l) when isTail expr_node ->
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let params = Symbol.getParamList callee_entry in
    let (quads1, revtemps) = List.fold_left
        (fun (quads, temps) e ->
           let unsolvedtyp = e.atom_typ in
           let typ = Typeinf.lookup_solved unsolvedtyp in
             if (isUnit typ) then 
               let (quads1, s_info) = gen_atom_stmt quads e in
               let quads2 = 
                 backpatch quads1 s_info.next_stmt (Label.nextLabel ())
               in
                 (quads2, O_Empty :: temps)
             else
               let (quads1, e_info) = gen_atom quads e in 
               let quads2 =
                 backpatch quads1 e_info.next_expr (Label.nextLabel ())
               in
               let temp = Quads.newTemp typ (Stack.top fun_stack) false in
               let quads3 = 
                 genQuad (Q_Assign, e_info.place, O_Empty, temp) quads2 
               in 
                 (quads3, temp :: temps)
        ) (quads, []) l
    in
    let quads2 =
      List.fold_left2
        (fun quads temp p ->
           if (temp = O_Empty) then 
             quads
           else
             let quads1 = 
               genQuad (Q_Assign, temp, O_Empty, O_Entry p) quads
             in
               quads1
        ) quads1 (List.rev revtemps) params
    in
    let jmp_target = Symbol.getFunctionLabel callee_entry in
    let () = addLabelTbl jmp_target in
    let quads3 = 
      genQuad (Q_Jump, O_Empty, O_Empty, O_Label jmp_target) quads2
    in
    let e_info = setExprInfo (O_Int 1) (Labels.newLabelList ()) in
      (quads3, e_info)
  | E_Id (id, l) -> 
    let quads1 =
      List.fold_left 
        (fun quads e ->
           if (isUnit e.atom_typ) then
             let (quads1, s_info) = gen_atom_stmt quads e in
             let quads2 = 
               backpatch quads1 s_info.next_stmt (Label.nextLabel ()) 
             in
               quads2
           else
             let (quads1, e_info) = gen_atom quads e in
             let quads2 = 
               backpatch quads1 e_info.next_expr (Label.nextLabel ()) 
             in
             let quads3 = 
               genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads2 
             in
               quads3
        )
        quads l
    in
    let fresh_typ = expr_node.expr_typ in
    let typ = Typeinf.lookup_solved fresh_typ in
    let temp = newTemp typ (Stack.top fun_stack) false in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let quads3 = 
      genQuad (Q_Call, O_Empty, O_Empty, O_Entry callee_entry) quads2 
    in
    let e_info = setExprInfo temp (Labels.newLabelList ()) in
      (quads3, e_info)
  | E_Block e -> gen_expr quads e
  | E_Ifthen (expr1, expr2) -> internal "If then from gen_expr"
  | E_Ifthenelse (expr1, expr2, expr3) ->
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (Label.nextLabel ()) in
    let (quads3, expr2_info) = gen_expr quads2 expr2 in
    let fresh_typ = expr_node.expr_typ in 
    let typ = Typeinf.lookup_solved fresh_typ in
    let temp = newTemp typ (Stack.top fun_stack) false in
    let quads4 = backpatch quads3 (expr2_info.next_expr) (Label.nextLabel ()) in
    let quads5 = genQuad (Q_Assign, expr2_info.place, O_Empty, temp) quads4 in
    let next = Labels.makeLabelList (Label.nextLabel ()) in
    let quads6 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads5 in
    let quads7 = backpatch quads6 (cond_info.false_lst) (Label.nextLabel ()) in
    let (quads8, expr3_info) = gen_expr quads7 expr3 in
    let quads9 = backpatch quads8 expr3_info.next_expr (Label.nextLabel ()) in
    let quads10 = genQuad(Q_Assign, expr3_info.place, O_Empty, temp) quads9 in
      (quads10, setExprInfo temp next)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let (funs, vars) = split_decls def in
    let quads1 = gen_decl quads vars delete_quads in
    let (quads2, expr_info) = gen_expr quads1 expr in
    let quads3 = gen_decl quads2 funs delete_quads in
    let quads4 = mergeQuads !delete_quads quads3 in
      (quads4, expr_info)
  | E_Dim (i, id) ->
    let temp = newTemp T_Int (Stack.top fun_stack) false in
    let id_entry = match expr_node.expr_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let obj = O_Entry id_entry in
    let dim = match i with 
      | None -> 1
      | Some i -> i
    in
    let quads1 = genQuad (Q_Dim, obj, O_Int dim, temp) quads in
      (quads1, setExprInfo temp (Labels.newLabelList ()))
  | E_New t -> 
    let size = sizeOfElement t in
    let new_entry = Symbol.findAuxilEntry "_new" in
    let temp = newTemp (T_Ref t) (Stack.top fun_stack) false in
    let quads1 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry new_entry) quads2 in
      (quads3, setExprInfo temp (Labels.newLabelList ()))
  | E_Atom a -> gen_atom quads a
  | E_While _ | E_For _ -> internal "While/For not expressions"
  | E_Match (expr, l) ->
    let (quads1, expr_info) = gen_expr quads expr in
    let quads2 = backpatch quads1 expr_info.next_expr (Label.nextLabel ()) in
    let fresh_res_typ = expr_node.expr_typ in
    let res_typ = Typeinf.lookup_solved fresh_res_typ in
    let temp = newTemp res_typ (Stack.top fun_stack) false in
    let (quads3, last) =
      List.fold_left (fun (quads, last) (Clause (patt, expr1)) ->
          let (quads1, cond_info) = gen_pattern patt expr_info.place quads in
          let quads2 = 
            backpatch quads1 cond_info.true_lst (Label.nextLabel ()) 
          in
          let (quads3, expr_info1) = gen_expr quads2 expr1 in
          let quads4 = 
            backpatch quads3 expr_info1.next_expr (Label.nextLabel ()) 
          in
          let quads5 = 
            genQuad (Q_Assign, expr_info1.place, O_Empty, temp) quads4 
          in
          let last = 
            Labels.mergeLabels last (Labels.makeLabelList (Label.nextLabel ())) 
          in
          let quads6 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads5 in
          let quads7 = 
            backpatch quads6 cond_info.false_lst (Label.nextLabel ()) 
          in
            (quads7, last)
        ) (quads2, Labels.newLabelList ()) l
    in
    let curr_fun_id = 
      let fn = (Stack.top fun_stack) in
        id_name fn.entry_id 
    in
    let err_str = 
      Printf.sprintf 
        "Match failure arising from partial matching in function %s\n" 
        curr_fun_id
    in 
    let quads4 = genQuad (Q_Par, O_Str err_str, O_ByVal, O_Empty) quads3 in
    let quads5 = 
      genQuad (Q_Call, O_Empty, O_Empty, O_Entry !Ast.print_string_entry) quads4 
    in 
    let quads6 = genQuad (Q_Fail, O_Empty, O_Empty, O_Empty) quads5 in
    let expr_info = setExprInfo temp last in
      (quads6, expr_info)
  | E_Cid (id, l) -> 
    let constructor_size e = match e.entry_info with 
      | ENTRY_constructor c ->
        List.fold_left (fun acc typ -> (sizeOfType typ) + acc) 
          (Types.tag_size) c.constructor_paramlist 
      | _ -> internal "Cannot find constructor size of non constructor entry"
    in
    let c_entry = match expr_node.expr_entry with 
      | Some e -> e
      | None -> internal "Expected entry"
    in
    (* allocate space for the constructor representation*)
    let ty = Symbol.getType c_entry in
    let size = constructor_size c_entry in
    let new_entry = Symbol.findAuxilEntry "_new" in
    let temp = newTemp (T_Ref ty) (Stack.top fun_stack) false in
    let quads1 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry new_entry) quads2 in
    (* store constructor tag and arguments *)
    let temp2 = newTemp (T_Ref T_Int) (Stack.top fun_stack) false in
    let quads4 = genQuad (Q_Constr, temp, O_Int 0, temp2) quads3 in
    let quads5 = 
      genQuad (Q_Assign, O_Int (Symbol.getTag c_entry), O_Empty, O_Deref temp2) 
        quads4 
    in
    let (quads6, temps) = 
      List.fold_left 
        (fun (quads, offset) a ->
           let (quads1, a_info) = gen_atom quads a in
           let quads2 = 
             backpatch quads1 a_info.next_expr (Label.nextLabel ()) 
           in
           let fresh_ty = a.atom_typ in
           let atom_ty = Typeinf.lookup_solved fresh_ty in
           let atom_temp = 
             newTemp (T_Ref atom_ty) (Stack.top fun_stack) false 
           in
           let quads3 = 
             genQuad (Q_Constr, temp, O_Int offset, atom_temp) quads2 
           in
           let quads4 = 
             genQuad (Q_Assign, a_info.place, O_Empty, O_Deref atom_temp) quads3 
           in
             (quads4, offset + (sizeOfType atom_ty))
        )
        (quads5, Types.tag_size) l
    in
      (quads6, setExprInfo temp (Labels.newLabelList ())) 
  | E_None -> internal "Dummy expression, must have been removed"

and gen_cond quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
    begin
      match op with
        | Plus | Minus | Times | Div | Mod
        | Fplus | Fminus | Ftimes | Fdiv | Power ->
          internal "Arithmetic operators cannot be conditions"
        | Seq | Nseq as oper ->
          let fresh_typ = expr1.expr_typ in
          let typ = Typeinf.lookup_solved fresh_typ in
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 e1_info.next_expr (Label.nextLabel ()) in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = 
            backpatch quads3 e2_info.next_expr (Label.nextLabel ()) in
            (match typ with 
              | T_Id id ->
                let u_entry = Symbol.lookupUdt (id_make id) in
                let eq_fun = Symbol.getEqFun u_entry in
                let res = newTemp T_Bool (Stack.top fun_stack) false in
                let quads5 =
                  genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads4 in
                let quads6 =
                  genQuad (Q_Par, e2_info.place, O_ByVal, O_Empty) quads5 in
                let quads7 =
                  genQuad (Q_Par, res, O_Ret, O_Empty) quads6 in
                let quads8 =
                  genQuad (Q_Call, O_Empty, O_Empty, O_Entry eq_fun) quads7 in
                let t = Labels.makeLabelList (Label.nextLabel ()) in
                let quads9 =
                  genQuad (Q_Ifb, res, O_Empty, O_Backpatch) quads8 in
                let f = Labels.makeLabelList (Label.nextLabel ()) in
                let quads10 =
                  genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads9 in
                let cond_info = match oper with
                  | Seq -> setCondInfo t f
                  | Nseq -> setCondInfo f t
                  | _ -> internal "Only structural eq/neq is supported"
                in
                  (quads10, cond_info)
              | typ ->
                let t = Labels.makeLabelList (Label.nextLabel ()) in
                let quads5 =
                  genQuad 
                    (getQuadBop oper, e1_info.place, e2_info.place, O_Backpatch) 
                    quads4
                in
                let f = Labels.makeLabelList (Label.nextLabel ()) in
                let quads6 = 
                  genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads5 
                in
                let cond_info = setCondInfo t f in
                  (quads6, cond_info) )
        | Eq | Neq | L 
        | Le | G | Ge as oper ->
          let fresh_typ = expr1.expr_typ in
          let typ = Typeinf.lookup_solved fresh_typ in
          let () = match typ with
            | T_Arrow _ | T_Array _ ->
              internal "this must not occur, it is checked int type inf";
            | _ -> ()
          in
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 e1_info.next_expr (Label.nextLabel ()) 
          in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = 
            backpatch quads3 e2_info.next_expr (Label.nextLabel ()) 
          in
          let t = Labels.makeLabelList (Label.nextLabel ()) in
          let quads5 = 
            genQuad (getQuadBop oper, e1_info.place, e2_info.place, O_Backpatch) 
              quads4
          in
          let f = Labels.makeLabelList (Label.nextLabel ()) in
          let quads6 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads5 in
          let cond_info = setCondInfo t f in
            (quads6, cond_info)
        | And -> 
          let (quads1, c1_info) = gen_cond quads expr1 in
          let quads2 = backpatch quads1 c1_info.true_lst (Label.nextLabel ()) in
          let (quads3, c2_info) = gen_cond quads2 expr2 in
          let t = c2_info.true_lst in
          let f = Labels.mergeLabels c1_info.false_lst c2_info.false_lst in
          let cond_info = setCondInfo t f in
            (quads3, cond_info) 
        | Or -> 
          let (quads1, c1_info) = gen_cond quads expr1 in
          let quads2 = 
            backpatch quads1 c1_info.false_lst (Label.nextLabel ()) 
          in
          let (quads3, c2_info) = gen_cond quads2 expr2 in
          let f = c2_info.false_lst in
          let t = Labels.mergeLabels c1_info.true_lst c2_info.true_lst in
          let cond_info = setCondInfo t f in
            (quads3, cond_info)
        | Semicolon ->
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = 
            backpatch quads1 stmt_info.next_stmt (Label.nextLabel ()) 
          in
          let (quads3, cond_info) = gen_cond quads2 expr2 in
            (quads3, cond_info)
        | Assign -> internal "Assign cannot be condition"
    end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Not -> 
          let (quads1, c_inf) = gen_cond quads expr1 in
          let t = c_inf.false_lst in
          let f = c_inf.true_lst in
          let cond_info = setCondInfo t f in
            (quads1, cond_info)
        | U_Plus | U_Minus | U_Fplus | U_Fminus | U_Del ->
          internal "Unary operators with no boolean type cannot be conditions"
    end
  | E_Ifthen (expr1, expr2) -> internal "If then from gen_cond"
  | E_Ifthenelse (expr1, expr2, expr3) ->
    let (quads1, cond1_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond1_info.true_lst (Label.nextLabel ()) in
    let (quads3, cond2_info) = gen_cond quads2 expr2 in
    let quads4 = backpatch quads3 (cond1_info.false_lst) (Label.nextLabel ()) in
    let (quads5, cond3_info) = gen_cond quads4 expr3 in
    let t = Labels.mergeLabels cond2_info.true_lst cond3_info.true_lst in
    let f = Labels.mergeLabels cond2_info.false_lst cond3_info.false_lst in
      (quads5, setCondInfo t f)
  | E_Block e -> gen_cond quads e 
  | E_Id (e, l) ->
    let (quads1, e_info) = gen_expr quads expr_node in
    let quads2 = backpatch quads1 e_info.next_expr (Label.nextLabel ()) in
    let true_lst = Labels.makeLabelList (Label.nextLabel ()) in
    let quads3 = genQuad (Q_Ifb, e_info.place, O_Empty, O_Backpatch) quads2 in
    let false_lst = Labels.makeLabelList (Label.nextLabel ()) in
    let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
    let cond_info = setCondInfo true_lst false_lst in
      (quads4, cond_info)
  | E_While _ | E_For _ | E_Dim _ | E_New _ | E_Cid _ ->
    internal "These expressions cannot have type bool"
  | E_Match (expr, l) -> 
    let (quads1, expr_info) = gen_expr quads expr in
    let quads2 = backpatch quads1 expr_info.next_expr (Label.nextLabel ()) in
    let (quads3, t, f) =
      List.fold_left (fun (quads, t, f) (Clause (patt, expr1)) ->
          let (quads1, cond_info) = gen_pattern patt expr_info.place quads in
          let quads2 = 
            backpatch quads1 cond_info.true_lst (Label.nextLabel ()) 
          in
          let (quads3, cond_info1) = gen_cond quads2 expr1 in
          let t = Labels.mergeLabels t cond_info1.true_lst in
          let f = Labels.mergeLabels f cond_info1.false_lst in
          let quads4 = 
            backpatch quads3 cond_info.false_lst (Label.nextLabel ()) 
          in
            (quads4, t, f)
        ) (quads2, Labels.newLabelList (), Labels.newLabelList ()) l
    in
    let curr_fun_id = 
      let fn = (Stack.top fun_stack) in
        id_name fn.entry_id 
    in
    let err_str = 
      Printf.sprintf 
        "Match failure arising from partial matching in function %s\n" 
        curr_fun_id
    in 
    let quads4 = genQuad (Q_Par, O_Str err_str, O_ByVal, O_Empty) quads3 in
    let quads5 = 
      genQuad (Q_Call, O_Empty, O_Empty, O_Entry !Ast.print_string_entry) quads4 
    in 

    let quads6 = genQuad (Q_Fail, O_Empty, O_Empty, O_Empty) quads5 in
    let cond_info = setCondInfo t f in
      (quads6, cond_info)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let (funs, vars) = split_decls def in
    let quads1 = gen_decl quads vars delete_quads in
    let (quads2, cond_info) = gen_cond quads1 expr in
    let quads3 = gen_decl quads2 funs delete_quads in
    let quads4 = mergeQuads !delete_quads quads3 in
      (quads4, cond_info)
  | E_Atom a ->
    begin
      match a.atom with
        | A_Num _ | A_Dec _ | A_Chr _ | A_Str _ | A_Par | A_Cid _ ->
          internal "Constants with non boolean type cannot be conditions"
        | A_Bool b ->
          let cond_lst = Labels.makeLabelList (Label.nextLabel ()) in
          let quads1 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads in
            (match b with
              | true -> (quads1, setCondInfo cond_lst (Labels.newLabelList ()))
              | false -> 
                (quads1, setCondInfo (Labels.newLabelList ()) cond_lst))
        | A_Var id -> 
          let true_lst = Labels.makeLabelList (Label.nextLabel ()) in
          let id_entry = match expr_node.expr_entry with
            | Some e -> e
            | None -> internal " Expected entry"
          in
          let quads1 = 
            genQuad (Q_Ifb, O_Entry id_entry, O_Empty, O_Backpatch) quads 
          in
          let false_lst = Labels.makeLabelList (Label.nextLabel ()) in
          let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
          let cond_info = setCondInfo true_lst false_lst in
            (quads2, cond_info)
        | A_Bang banged -> 
          let (quads1, expr_info) = gen_atom quads a in   
          let quads2 = 
            backpatch quads1 (expr_info.next_expr) (Label.nextLabel ()) 
          in
          let true_lst = Labels.makeLabelList (Label.nextLabel ()) in
          let quads3 = 
            genQuad (Q_Ifb, expr_info.place, O_Empty, O_Backpatch) quads2 
          in
          let false_lst = Labels.makeLabelList (Label.nextLabel ()) in
          let quads4 = genQuad(Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let cond_info = setCondInfo true_lst false_lst in
            (quads4, cond_info)
        | A_Array _ ->
          internal "Expression with reference type cannot be a condition"
        | A_Expr e -> gen_cond quads e
        | A_None -> internal "Dummy atom, must have been removed"
    end
  | E_None -> internal "Dummy expression, must have been removed"

and gen_stmt quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
    begin 
      match op with 
        | Plus | Minus | Times | Div | Mod
        | Fplus | Fminus | Ftimes | Fdiv | Power -> 
          let (quads1, e1_info) = gen_expr quads expr_node in
          let s_info = setStmtInfo (e1_info.next_expr) in 
            (quads1, s_info)
        | Seq | Nseq | Eq | Neq
        | L | Le | G | Ge 
        | And | Or ->
          let (quads1, cond_info) = gen_cond quads expr_node in
          let n = Labels.mergeLabels cond_info.true_lst cond_info.false_lst in
          let s_info = setStmtInfo n in
            (quads1, s_info)
        | Semicolon     -> 
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = 
            backpatch quads1 stmt_info.next_stmt (Label.nextLabel ()) 
          in
          let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
            (quads3, stmt2_info)
        | Assign        -> 
          let (quads1, expr1_info) = gen_expr quads expr1 in
          let quads2 = 
            backpatch quads1 expr1_info.next_expr (Label.nextLabel ()) 
          in
            if (isUnit expr2.expr_typ) 
            then
              let (quads3, s_info) = gen_stmt quads2 expr2 in
              let n = s_info.next_stmt in
                (quads2, setStmtInfo n)
            else
              let (quads3, expr2_info) = gen_expr quads2 expr2 in
              let quads4 = 
                backpatch quads3 expr2_info.next_expr (Label.nextLabel ()) 
              in
              let quads5 = 
                genQuad 
                  (Q_Assign, expr2_info.place, O_Empty, O_Deref expr1_info.place) 
                  quads4 
              in
              let n = Labels.newLabelList () in
                (quads5, setStmtInfo n)
    end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Plus | U_Minus | U_Fplus | U_Fminus ->
          let (quads1, e_info) = gen_expr quads expr_node in
            (quads1, setStmtInfo e_info.next_expr)
        | U_Del -> 
          let (quads1, e1_info) = gen_expr quads expr1 in
          let delete_entry = Symbol.findAuxilEntry "_delete" in
          let quads2 = 
            backpatch quads1 e1_info.next_expr (Label.nextLabel ()) 
          in
          let quads3 = 
            genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads2 
          in
          let quads4 = 
            genQuad (Q_Call, O_Empty, O_Empty, O_Entry delete_entry) quads3 
          in
          let s_info = setStmtInfo (Labels.newLabelList ()) in
            (quads4, s_info)
        | U_Not -> 
          let (quads1, cond_info) = gen_cond quads expr_node in 
          let next = 
            Labels.mergeLabels cond_info.true_lst cond_info.false_lst 
          in
            (quads1, setStmtInfo next)
    end
  | E_Ifthen (expr1, expr2) -> 
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (Label.nextLabel ()) in
    let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
    let next = Labels.mergeLabels cond_info.false_lst stmt2_info.next_stmt in
      (quads3, setStmtInfo next)
  | E_Ifthenelse (expr1, expr2, expr3) ->
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (Label.nextLabel ()) in
    let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
    let l1 = Labels.makeLabelList (Label.nextLabel ()) in
    let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
    let quads5 = backpatch quads4 (cond_info.false_lst) (Label.nextLabel ()) in
    let (quads6, stmt3_info) = gen_stmt quads5 expr3 in
    let merged = Labels.mergeLabels l1 stmt2_info.next_stmt in 
    let next = Labels.mergeLabels stmt3_info.next_stmt merged in
      (quads6, setStmtInfo next)
  | E_While (expr1, expr2) ->
    let q = Label.nextLabel () in
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (Label.nextLabel ()) in
    let (quads3, stmt_info) = gen_stmt quads2 expr2 in
    let quads4 = backpatch quads3 stmt_info.next_stmt q in
    let quads5 = genQuad (Q_Jump, O_Empty, O_Empty, O_Label q) quads4 in
    let next = setStmtInfo (cond_info.false_lst) in
      addLabelTbl q;
      (quads5, next)
  | E_For (id, expr1, cnt, expr2, expr3) ->
    let (relop, op) = match cnt with 
      | To -> (Q_Le, Q_Plus)
      | Downto -> (Q_Ge, Q_Minus) 
    in
    let (quads1, expr1_info) = gen_expr quads expr1 in
    let quads2 = backpatch quads1 expr1_info.next_expr (Label.nextLabel ()) in
    let id_entry = match expr_node.expr_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let obj = O_Entry id_entry in
    let quads3 = genQuad (Q_Assign, expr1_info.place, O_Empty, obj) quads2 in
    let (quads4, expr2_info) = gen_expr quads3 expr2 in
    let quads5 = backpatch quads4 expr2_info.next_expr (Label.nextLabel ()) in
    let condLabel = Label.nextLabel () in
    let l1 = Labels.makeLabelList condLabel in
    let quads6 = genQuad (relop, obj, expr2_info.place, O_Backpatch) quads5 in
    let next = Labels.makeLabelList (Label.nextLabel ()) in 
    let quads7 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads6 in
    let quads8 = backpatch quads7 l1 (Label.nextLabel ()) in
    let (quads9, stmt_info) = gen_stmt quads8 expr3 in
    let quads10 = backpatch quads9 stmt_info.next_stmt (Label.nextLabel ()) in
    let quads11 = genQuad (op, obj, O_Int 1, obj) quads10 in
    let quads12 = 
      genQuad (Q_Jump, O_Empty, O_Empty, O_Label condLabel) quads11 
    in
      addLabelTbl condLabel;
      (quads12, setStmtInfo next)
  | E_Id (id, l) when isTail expr_node ->
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let params = Symbol.getParamList callee_entry in
    let (quads1, revtemps) = List.fold_left
        (fun (quads, temps) e ->
           let unsolvedtyp = e.atom_typ in
           let typ = Typeinf.lookup_solved unsolvedtyp in
             if (isUnit typ) then 
               let (quads1, s_info) = gen_atom_stmt quads e in
               let quads2 = 
                 backpatch quads1 s_info.next_stmt (Label.nextLabel ())
               in
                 (quads2, O_Empty :: temps)
             else
               let (quads1, e_info) = gen_atom quads e in 
               let quads2 =
                 backpatch quads1 e_info.next_expr (Label.nextLabel ())
               in
               let temp = Quads.newTemp typ (Stack.top fun_stack) false in
               let quads3 = 
                 genQuad (Q_Assign, e_info.place, O_Empty, temp) quads2 
               in 
                 (quads3, temp :: temps)
        ) (quads, []) l
    in
    let quads2 =
      List.fold_left2
        (fun quads temp p ->
           if (temp = O_Empty) then 
             quads
           else
             let quads1 = 
               genQuad (Q_Assign, temp, O_Empty, O_Entry p) quads
             in
               quads1
        ) quads1 (List.rev revtemps) params
    in
    let jmp_target = Symbol.getFunctionLabel callee_entry in
    let () = addLabelTbl jmp_target in
    let quads3 = 
      genQuad (Q_Jump, O_Empty, O_Empty, O_Label jmp_target) quads2
    in
    let s_info = setStmtInfo (Labels.newLabelList ()) in
      (quads3, s_info)
  | E_Id (id, l) -> 
    let quads1 =
      List.fold_left 
        (fun quads e ->
           if (isUnit e.atom_typ)
           then
             let (quads1, s_info) = gen_atom_stmt quads e in
             let quads2 = 
               backpatch quads1 s_info.next_stmt (Label.nextLabel ()) 
             in
               quads2
           else
             let (quads1, e_info) = gen_atom quads e in
             let quads2 = 
               backpatch quads1 e_info.next_expr (Label.nextLabel ()) 
             in
             let quads3 = 
               genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads2 
             in
               quads3
        )
        quads l
    in
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let quads2 = 
      genQuad (Q_Call, O_Empty, O_Empty, O_Entry callee_entry) quads1 
    in
    let s_info = setStmtInfo (Labels.newLabelList ()) in
      (quads2, s_info)
  | E_Dim _ -> 
    let (quads1, e_info) = gen_expr quads expr_node in
      (quads1, setStmtInfo e_info.next_expr)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let (funs, vars) = split_decls def in
    let quads1 = gen_decl quads vars delete_quads in
    let (quads2, stmt_info) = gen_stmt quads1 expr in
    let quads3 = gen_decl quads2 funs delete_quads in
    let quads4 = mergeQuads !delete_quads quads3 in
      (quads4, stmt_info)
  | E_New t ->
    let (quads1, expr_info) = gen_expr quads expr_node in
      (quads1, setStmtInfo expr_info.next_expr)
  | E_Match (expr, l) -> 
    let (quads1, expr_info) = gen_expr quads expr in
    let quads2 = backpatch quads1 expr_info.next_expr (Label.nextLabel ()) in
    let (quads3, last) =
      List.fold_left (fun (quads, last) (Clause (patt, expr1)) ->
          let (quads1, cond_info) = gen_pattern patt expr_info.place quads in
          let quads2 = 
            backpatch quads1 cond_info.true_lst (Label.nextLabel ()) 
          in
          let (quads3, stmt_info1) = gen_stmt quads2 expr1 in
          let quads4 = 
            backpatch quads3 stmt_info1.next_stmt (Label.nextLabel ()) 
          in
          let last = 
            Labels.mergeLabels last (Labels.makeLabelList (Label.nextLabel ())) 
          in
          let quads5 = 
            genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads4 
          in
          let quads6 = 
            backpatch quads5 cond_info.false_lst (Label.nextLabel ()) 
          in
            (quads6, last)
        ) (quads2, Labels.newLabelList ()) l
    in
    let curr_fun_id = 
      let fn = (Stack.top fun_stack) in
        id_name fn.entry_id 
    in
    let err_str = 
      Printf.sprintf 
        "Match failure arising from partial matching in function %s\n" 
        curr_fun_id
    in 
    let quads4 = genQuad (Q_Par, O_Str err_str, O_ByVal, O_Empty) quads3 in
    let quads5 = 
      genQuad (Q_Call, O_Empty, O_Empty, O_Entry !Ast.print_string_entry) quads4 
    in 

    let quads6 = genQuad (Q_Fail, O_Empty, O_Empty, O_Empty) quads5 in
    let stmt_info = setStmtInfo last in
      (quads6, stmt_info)
  | E_Cid _ ->  internal "Constructors cannot be a statement"
  | E_Block e -> gen_stmt quads e
  | E_Atom a -> gen_atom_stmt quads a
  | E_None -> internal "Dummy expression, must have been removed"


and gen_atom quads atom_node = match atom_node.atom with
  | A_Num n -> (quads, setExprInfo (O_Int n) (Labels.newLabelList ()))
  | A_Dec f -> (quads, setExprInfo (O_Float f) (Labels.newLabelList ()))  
  | A_Chr c -> (quads, setExprInfo (O_Char c) (Labels.newLabelList ()))
  | A_Str str -> (quads, setExprInfo (O_Str str) (Labels.newLabelList ()))
  | A_Bool b -> (quads, setExprInfo (O_Bool b) (Labels.newLabelList ()))
  | A_Cid cid -> 
    let c_entry = match atom_node.atom_entry with
      | Some e -> e
      | None -> internal "Expected entry"
    in
    let ty = Symbol.getType c_entry in
    let size = sizeOfType ty in
    let new_entry = Symbol.findAuxilEntry "_new" in
    let temp = newTemp (T_Ref ty) (Stack.top fun_stack) false in
    let quads1 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry new_entry) quads2 in
    let temp2 = newTemp (T_Ref T_Int) (Stack.top fun_stack) false in
    let quads4 = genQuad (Q_Constr, temp, O_Int 0, temp2) quads3 in
    let quads5 = 
      genQuad (Q_Assign, O_Int (Symbol.getTag c_entry), O_Empty, O_Deref temp2) 
        quads4 
    in
      (quads5, setExprInfo temp (Labels.newLabelList ()))
  | A_Var v -> 
    let var_entry = match atom_node.atom_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let place = O_Entry var_entry in
      (quads, setExprInfo place (Labels.newLabelList ()))
  | A_Par -> internal "Reached unreachable point. Unit in gen_atom.\n"
  | A_Bang a -> 
    let (quads1, expr_info) = gen_atom quads a in   
    let fresh_typ = a.atom_typ in
    let typ = Typeinf.lookup_solved fresh_typ  in
    let temp = newTemp typ (Stack.top fun_stack) false in
    let quads2 = backpatch quads1 (expr_info.next_expr) (Label.nextLabel ()) in
    let quads3 = genQuad (Q_Assign, expr_info.place, O_Empty, temp) quads2 in
    let place = O_Deref temp in
      (quads3, setExprInfo place (Labels.newLabelList ()))
  | A_Array (id, expr_list) ->
    let fresh_typ = atom_node.atom_typ in
    let typ = Typeinf.lookup_solved fresh_typ  in
    let array_entry = match atom_node.atom_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let (quads1, temps) = 
      List.fold_left 
        (fun (quads, acc) e ->
           let (quads1, e_info1) = gen_expr quads e in
           let quads2 = 
             backpatch quads1 e_info1.next_expr (Label.nextLabel ()) 
           in
           let temp = newTemp T_Int (Stack.top fun_stack) false in
           let quads3 = 
             genQuad (Q_Assign, e_info1.place, O_Empty, temp) quads2 
           in
             (quads3, temp :: acc)
        )
        (quads, []) expr_list
    in
    let temp = newTemp typ (Stack.top fun_stack) false in
    let e_info = setExprInfo temp (Labels.newLabelList ()) in
    let quads2 = 
      genQuad (Q_Array, O_Entry array_entry, O_Index temps, temp) quads1 
    in 
      (quads2, e_info)
  | A_Expr e ->
    gen_expr quads e
  | A_None -> internal "Dummy atom, must have been removed"

and gen_atom_stmt quads atom_node = match atom_node.atom with
  | A_Num _ | A_Dec _ | A_Str _
  | A_Chr _ | A_Bool _ | A_Array _ -> 
    internal "non unit expressions called from atom_stmt"
  | A_Par | A_Var _ -> (quads, setStmtInfo (Labels.newLabelList ()))
  | A_Expr e -> gen_stmt quads e
  | A_Bang a -> 
    let (quads1, expr_info) = gen_atom quads a in   
    let n = expr_info.next_expr in
      (quads1, setStmtInfo n)
  | A_Cid _ -> internal "Constructors cannot be a statement"
  | A_None -> internal "Dummy atom, must have been removed"

and gen_pattern pat scrut quads =
  match pat.pattern with
    | Pa_Atom a -> gen_pattom a scrut quads
    | Pa_Cid (cid, l) ->
      let c_entry = match pat.pattern_entry with
        | Some e -> e
        | None -> internal "yet another match............"
      in
      let t = Labels.makeLabelList (Label.nextLabel ()) in
      let quads1 = 
        genQuad (Q_Match, scrut, O_Entry c_entry, O_Backpatch) quads 
      in
      let f = Labels.makeLabelList (Label.nextLabel ()) in
      let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
      let (quads3, _, t, f) =
        List.fold_left (fun (quads, offset, t, f) patt ->
            let fresh_ty = patt.pattom_typ in
            let ty = Typeinf.lookup_solved fresh_ty in
            let reftemp = newTemp (T_Ref ty) (Stack.top fun_stack) false in
            let temp = newTemp ty (Stack.top fun_stack) false in
            let quads1 = backpatch quads t (Label.nextLabel ()) in
            let quads2 = 
              genQuad (Q_Constr, scrut, O_Int offset, reftemp) quads1 
            in
            let quads3 = 
              genQuad (Q_Assign, O_Deref reftemp, O_Empty, temp) quads2 
            in
            let (quads4, cond_info) = gen_pattom patt temp quads3 in
            let f = Labels.mergeLabels f cond_info.false_lst in
              (quads4, offset + (sizeOfType ty), cond_info.true_lst, f) 
          ) (quads2, tag_size, t, f) l
      in
      let cond_info = setCondInfo t f in
        (quads3, cond_info)

and gen_pattom pat scrut quads =
  match pat.pattom with
    | P_Sign _ | P_Fsign _ as op ->
      let op = match op with
        | P_Sign (P_Plus, num) -> O_Int num
        | P_Sign (P_Minus, num) -> O_Int (-num)
        | P_Fsign (P_Fplus, num) -> O_Float num
        | P_Fsign (P_Fminus, num) -> O_Float (-. num)
        | _ -> internal "Must be a sign pattern"
      in
      let t = Labels.makeLabelList (Label.nextLabel ()) in
      let quads1 = genQuad (Q_Seq, scrut, op, O_Backpatch) quads in
      let f = Labels.makeLabelList (Label.nextLabel ()) in
      let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
      let cond_info = setCondInfo t f in
        (quads2, cond_info)
    | P_Num _ | P_Float _ | P_Chr _ | P_Bool _ as op ->
      let operand = match op with
        | P_Num n -> O_Int n
        | P_Float f -> O_Float f
        | P_Chr c -> O_Char c
        | P_Bool b -> O_Bool b
        | _ -> internal "Pattern should be one of the op"
      in
      let t = Labels.makeLabelList (Label.nextLabel ()) in
      let quads1 = genQuad (Q_Seq, scrut, operand, O_Backpatch) quads in
      let f = Labels.makeLabelList (Label.nextLabel ()) in
      let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
      let cond_info = setCondInfo t f in
        (quads2, cond_info)
    | P_Id id ->
      let c_entry = match pat.pattom_entry with
        | Some e -> e
        | None -> internal "yet another match............"
      in
      let () = update_entry_typ c_entry in
      let quads1 = genQuad (Q_Assign, scrut, O_Empty, O_Entry c_entry) quads in
      let t = Labels.newLabelList () in
      (*let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in*)
      let f = Labels.newLabelList () in
      let cond_info = setCondInfo t f in
        (quads1, cond_info)
    | P_Cid cid -> 
      let c_entry = match pat.pattom_entry with
        | Some e -> e
        | None -> internal "yet another match............"
      in
      let t = Labels.makeLabelList (Label.nextLabel ()) in
      let quads1 = 
        genQuad (Q_Match, scrut, O_Entry c_entry, O_Backpatch) quads in
      let f = Labels.makeLabelList (Label.nextLabel ()) in
      let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
      let cond_info = setCondInfo t f in
        (quads2, cond_info)
    | P_Pattern p -> gen_pattern p scrut quads

