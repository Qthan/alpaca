open Identifier
open Types
open AstTypes
open SymbTypes
open Symbol
open Typeinf
open Quads
open Error

(*  Symbol entry option to unsolved type *)
let lookup_type entry =
  match entry with
    | None -> internal "Entry not found\n"
    | Some e -> getType e

let lookup_res_type entry =
  match entry with
    | None -> internal "Entry not found\n"
    | Some e -> getResType e

let update_entry_typ entry =
  let fresh_typ = getResType entry in
  let typ = lookup_solved fresh_typ in
    setType entry typ

let update_def_typ def = 
  match def.def_entry with 
    | None -> internal "Definition must have an entry"
    | Some e -> 
      update_entry_typ e;
      match e.entry_info with
        | ENTRY_function f -> List.iter update_entry_typ f.function_paramlist
        | _ -> ()

(* Function's local variables size*)
let get_var_size entry =
  match entry.entry_info with
    | ENTRY_function f -> f.function_varsize
    | _ -> internal "Looked for local variables of - Some thing - that's not a function"

(* XXX Check if type is unit XXX*)
let isUnit fresh_typ = 
  match (lookup_solved fresh_typ) with 
    | T_Unit -> true
    | _ -> false

let offset_stack = Stack.create () 

let rec gen_program ast subst outer_entry =
  let quads = gen_decl_list ast outer_entry in
  let finalQuads = normalizeQuads (List.rev quads) in
    printQuads finalQuads;
    finalQuads

and gen_decl_list ast outer_entry = 
  let delete_quads = ref (newQuadList ()) in
  let () = fixOffsets outer_entry in              (* Stupid Some *)
  let outer_size = get_var_size outer_entry in
  let () = Stack.push outer_size offset_stack in
  let outer = genQuad (Q_Unit, O_Entry outer_entry, O_Empty, O_Empty) (newQuadList ()) in
  let quads = List.fold_left (fun o a -> gen_decl o a delete_quads) outer ast in
  let quads2 = mergeQuads !delete_quads quads in
  let final = genQuad (Q_Endu, O_Entry outer_entry, O_Empty, O_Empty) quads2 in
    final

and gen_decl outer stmt delete_quads = match stmt with
  | S_Let l | S_Rec l -> 
    List.iter update_def_typ l;
    gen_def_list outer l delete_quads
  | S_Type l -> outer (* dummy value, kouo-kouo-kouo dld kotopouleiro*)

and gen_def_list outer lst delete_quads = List.fold_left (fun o l -> gen_def o l delete_quads ) outer lst

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
                  let quads2 = backpatch quads1 s_info.next_stmt (nextLabel ()) in
                    quads2
                else
                  let (quads1, e_info) = gen_expr quads expr in
                  let quads2 = backpatch quads1 e_info.next_expr (nextLabel ()) in
                  let quads3 = genQuad (Q_Assign, e_info.place, O_Empty, O_Entry entry) quads2 in
                    quads3
            | (id, _) :: params ->
              let () = fixOffsets entry in
              let typ = lookup_res_type (Some entry) in
              let locals_size = get_var_size entry in
              let fQuads = newQuadList () in
              let () = Stack.push locals_size offset_stack in
              let fQuads1 = 
                genQuad (Q_Unit, O_Entry entry, O_Empty, O_Empty) fQuads 
              in
                if (isUnit typ)
                then
                  let (fQuads2, s_info) = gen_stmt fQuads1 expr in
                  let fQuads3 = backpatch fQuads2 s_info.next_stmt (nextLabel ()) in
                  let fQuads4 = genQuad (Q_Endu, O_Entry entry, O_Empty, O_Empty) fQuads3 in 
                  let _ = Stack.pop offset_stack in
                    mergeQuads quads fQuads4 
                else
                  let (fQuads2, e_info) = gen_expr fQuads1 expr in
                  let fQuads3 = backpatch fQuads2 e_info.next_expr (nextLabel ()) in
                  let fQuads4 = genQuad (Q_Assign, e_info.place, O_Empty, O_Res) fQuads3
                  in
                  let fQuads5 = genQuad (Q_Endu, O_Entry entry, O_Empty, O_Empty) fQuads4 in 
                  let _ = Stack.pop offset_stack in
                    mergeQuads quads fQuads5 
        end
      | D_Mut (_, _)    -> quads
      | D_Array (id, _, lst)  -> 
        let rec gen_array_dims lst quads =
          match lst with
            | [] -> quads  (* XXX: maybe internal here *)
            | dim :: xs ->
              let (quads1, e_info) = gen_expr quads dim in
              let quads2 = genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads1 in
                gen_array_dims xs quads2
        in
        let quads1 = gen_array_dims lst quads in
        let ty = lookup_type def_node.def_entry in
        let size = sizeOfType ty in  (* Size of an array element *)
        let quads2 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads1 in
        let makearr_entry = findAuxilEntry "_make_array" in
        let delete_entry =  findAuxilEntry "_delete_array" in
        let dims = (* --- Need to get an int out of Dim type --- *)
          match arrayDims ty with
            | D_Int d -> d
            | D_Alpha _ -> internal "Unknown array dimensions\n"
        in
        let quads3 = genQuad (Q_Par, O_Int dims, O_ByVal, O_Empty) quads2 in
        let quads4 = genQuad (Q_Par, O_Entry entry , O_Ret, O_Empty) quads3 in (* --- changed Obj type --- *)
        let quads5 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry makearr_entry) quads4 in
          delete_quads := genQuad (Q_Par, O_Entry entry, O_ByVal, O_Empty) !delete_quads;
          delete_quads := genQuad (Q_Call, O_Empty, O_Empty, O_Entry delete_entry) !delete_quads;
          quads5

and gen_expr quads expr_node = match expr_node.expr with 
  | E_Binop (expr1, op, expr2) ->
    begin 
      match op with 
        | Plus | Minus | Times | Div | Mod
        | Fplus | Fminus | Ftimes | Fdiv | Power as oper -> 
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 (e1_info.next_expr) (nextLabel ()) in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = backpatch quads3 (e2_info.next_expr) (nextLabel ()) in
          let typ = expr_node.expr_typ in
          (* let solved = lookup_solved typ  in -- reduntant*) 
          let temp = newTemp typ (Stack.top offset_stack) in
          let quads5 = 
            genQuad (getQuadBop oper, e1_info.place, e2_info.place, temp) quads4
          in
          let e_info = setExprInfo temp (newLabelList ()) in
            (quads5, e_info)
        | Seq | Nseq | Eq | Neq
        | L | Le | G | Ge 
        | And | Or ->
          let (quads1, cond_info) = gen_cond quads expr_node in
          let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
          let temp = newTemp expr_node.expr_typ (Stack.top offset_stack) in
          let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
          let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
          let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
          let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
            (quads6, e_info)
        | Semicolon -> 
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = backpatch quads1 stmt_info.next_stmt (nextLabel ()) in
          let (quads3, expr_info) = gen_expr quads2 expr2 in
            (quads3, expr_info)
        | Assign -> internal "Assign is not an expression\n"
    end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Plus | U_Minus | U_Fplus | U_Fminus as oper ->
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 e1_info.next_expr (nextLabel ()) in
          let typ = expr_node.expr_typ in
          let temp = newTemp typ (Stack.top offset_stack) in
          let quads3 = match typ with
            | T_Int -> genQuad (getQuadUnop oper, O_Int 0, e1_info.place, temp) quads2
            | T_Float -> genQuad (getQuadUnop oper, O_Float 0., e1_info.place, temp) quads2
          in
          let e_info = setExprInfo temp (newLabelList ()) in
            (quads3, e_info)
        | U_Del -> internal "Unreachable point: Delete is not an expression"
                     (* Will create gen_stm and U_Del will go there*)
                     (*let (quads1, e1_info) = gen_expr quads expr1 in
                       let quads2 = genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads1 in
                       let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Fun "_delete") in
                       let e_info = 
                       (quads3, e_info)*)
                     failwith "not an expression"
        | U_Not ->
          let (quads1, cond_info) = gen_cond quads expr_node in
          let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
          let temp = newTemp expr_node.expr_typ (Stack.top offset_stack) in
          let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
          let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
          let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
          let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
            (quads6, e_info)
    end 
  | E_Id (id, l) -> 
    let quads1 =
      List.fold_left 
        (fun quads e ->
           if (isUnit e.atom_typ)
           then
             let (quads1, s_info) = gen_atom_stmt quads e in
             let quads2 = backpatch quads1 s_info.next_stmt (nextLabel ()) in
               quads2
           else
             let (quads1, e_info) = gen_atom quads e in
             let quads2 = backpatch quads1 e_info.next_expr (nextLabel ()) in
             let quads3 = genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads2 in
               quads3
        )
        quads l
    in
    let fresh_typ = expr_node.expr_typ in
    let typ = lookup_solved fresh_typ  in
    let temp = newTemp typ (Stack.top offset_stack) in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry callee_entry) quads2 in
    let e_info = setExprInfo temp (newLabelList ()) in
      (quads3, e_info)
  | E_Block e -> gen_expr quads e
  | E_Ifthen (expr1, expr2) -> internal "If then from gen_expr"
  | E_Ifthenelse (expr1, expr2, expr3) ->
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
    let (quads3, expr2_info) = gen_expr quads2 expr2 in
    let fresh_typ = expr_node.expr_typ in 
    let typ = lookup_solved fresh_typ  in
    let temp = newTemp typ (Stack.top offset_stack) in
    let quads4 = genQuad (Q_Assign, expr2_info.place, O_Empty, temp) quads3 in
    let l1 = makeLabelList (nextLabel ()) in
    let quads5 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads4 in
    let quads6 = backpatch quads5 (cond_info.false_lst) (nextLabel ()) in
    let (quads7, expr3_info) = gen_expr quads6 expr3 in
    let quads8 = backpatch quads7 expr3_info.next_expr (nextLabel ()) in
    let quads9 = genQuad(Q_Assign, expr3_info.place, O_Empty, temp) quads8 in
    let next = mergeLabels expr2_info.next_expr l1 in
      (quads9, setExprInfo temp next)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let quads1 = gen_decl quads def delete_quads in
    let (quads2, expr_info) = gen_expr quads1 expr in
    let quads3 = mergeQuads !delete_quads quads2 in
      (quads3, expr_info)
  | E_Dim (i, id) ->
    let temp = newTemp T_Int (Stack.top offset_stack) in
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
      (quads1, setExprInfo temp (newLabelList ()))
  | E_New t -> 
    let size = sizeOfType t in
    let new_entry = findAuxilEntry "_new" in
    let temp = newTemp (T_Ref t) (Stack.top offset_stack) in
    let quads1 = genQuad (Q_Par, O_Int size, O_ByVal, O_Empty) quads in
    let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
    let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry new_entry) quads2 in
      (quads3, setExprInfo temp (newLabelList ()))
  | E_Atom a -> gen_atom quads a
  | E_While _ | E_For _ -> internal "While/For not expressions"
  | E_Match _ | E_Cid _ -> (quads, setExprInfo (O_Empty) (newLabelList ()))   (* dummy return value TODO *)

and gen_cond quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
    begin
      match op with
        | Plus | Minus | Times | Div | Mod
        | Fplus | Fminus | Ftimes | Fdiv | Power ->
          internal "Arithmetic operators cannot be conditions"
        | Seq | Nseq | Eq | Neq
        | L | Le | G | Ge as oper ->
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 e1_info.next_expr (nextLabel ()) in
          let (quads3, e2_info) = gen_expr quads2 expr2 in
          let quads4 = backpatch quads3 e2_info.next_expr (nextLabel ()) in
          let t = makeLabelList (nextLabel ()) in
          let quads5 = 
            genQuad (getQuadBop oper, e1_info.place, e2_info.place, O_Backpatch) quads4
          in
          let f = makeLabelList (nextLabel ()) in
          let quads6 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads5 in
          let cond_info = setCondInfo t f in
            (quads6, cond_info)
        | And -> 
          let (quads1, c1_info) = gen_cond quads expr1 in
          let quads2 = backpatch quads1 c1_info.true_lst (nextLabel ()) in
          let (quads3, c2_info) = gen_cond quads2 expr2 in
          let t = c2_info.true_lst in
          let f = mergeLabels c1_info.false_lst c2_info.false_lst in
          let cond_info = setCondInfo t f in
            (quads3, cond_info) 
        | Or -> 
          let (quads1, c1_info) = gen_cond quads expr1 in
          let quads2 = backpatch quads1 c1_info.false_lst (nextLabel ()) in
          let (quads3, c2_info) = gen_cond quads2 expr2 in
          let f = c2_info.false_lst in
          let t = mergeLabels c1_info.true_lst c2_info.true_lst in
          let cond_info = setCondInfo t f in
            (quads3, cond_info)
        | Semicolon ->
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = backpatch quads1 stmt_info.next_stmt (nextLabel ()) in
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
    let quads2 = backpatch quads1 cond1_info.true_lst (nextLabel ()) in
    let (quads3, cond2_info) = gen_cond quads2 expr2 in
    let quads4 = backpatch quads3 (cond1_info.false_lst) (nextLabel ()) in
    let (quads5, cond3_info) = gen_cond quads4 expr3 in
    let t = mergeLabels cond2_info.true_lst cond3_info.true_lst in
    let f = mergeLabels cond2_info.false_lst cond3_info.false_lst in
      (quads5, setCondInfo t f)
  | E_Block e -> gen_cond quads e 
  | E_Id (e, l) ->
    let (quads1, e_info) = gen_expr quads expr_node in
    let quads2 = backpatch quads1 e_info.next_expr (nextLabel ()) in
    let true_lst = makeLabelList (nextLabel ()) in
    let quads3 = genQuad (Q_Ifb, e_info.place, O_Empty, O_Backpatch) quads2 in
    let false_lst = makeLabelList (nextLabel ()) in
    let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
    let cond_info = setCondInfo true_lst false_lst in
      (quads4, cond_info)
  | E_While _ | E_For _ | E_Dim _ | E_New _ | E_Cid _ ->
    internal "These expressions cannot have type bool"
  | E_Match _ -> (quads, setCondInfo (newLabelList ()) (newLabelList ()))  (* TODO dummy return value XXX *)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let quads1 = gen_decl quads def delete_quads in
    let (quads2, cond_info) = gen_cond quads1 expr in
    let quads3 = mergeQuads !delete_quads quads2 in
      (quads3, cond_info)
  | E_Atom a ->
    begin
      match a.atom with
        | A_Num _ | A_Dec _ | A_Chr _ | A_Str _ | A_Par | A_Cid _ ->
          internal "Constants with non boolean type cannot be conditions"
        | A_Bool b ->
          let cond_lst = makeLabelList (nextLabel ()) in
          let quads1 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads in
            (match b with
              | true -> (quads1, setCondInfo cond_lst (newLabelList ()))
              | false -> (quads1, setCondInfo (newLabelList ()) cond_lst))
        | A_Var id -> 
          let true_lst = makeLabelList (nextLabel ()) in
          let id_entry = match expr_node.expr_entry with
            | Some e -> e
            | None -> internal " Expected entry"
          in
          let quads1 = genQuad (Q_Ifb, O_Entry id_entry, O_Empty, O_Backpatch) quads in
          let false_lst = makeLabelList (nextLabel ()) in
          let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
          let cond_info = setCondInfo true_lst false_lst in
            (quads2, cond_info)
        | A_Bang banged -> 
          let (quads1, expr_info) = gen_atom quads a in   
          let quads2 = backpatch quads1 (expr_info.next_expr) (nextLabel ()) in
          let true_lst = makeLabelList (nextLabel ()) in
          let quads3 = genQuad (Q_Ifb, expr_info.place, O_Empty, O_Backpatch) quads2 in
          let false_lst = makeLabelList (nextLabel ()) in
          let quads4 = genQuad(Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let cond_info = setCondInfo true_lst false_lst in
            (quads4, cond_info)
        | A_Array _ ->
          internal "Expression with reference type cannot be a condition"
        | A_Expr e -> gen_cond quads e
    end

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
          let n = mergeLabels cond_info.true_lst cond_info.false_lst in
          let s_info = setStmtInfo n in
            (quads1, s_info)
        | Semicolon     -> 
          let (quads1, stmt_info) = gen_stmt quads expr1 in
          let quads2 = backpatch quads1 stmt_info.next_stmt (nextLabel ()) in
          let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
            (quads3, stmt2_info)
        | Assign        -> 
          let (quads1, expr1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 expr1_info.next_expr (nextLabel ()) in
            if (isUnit expr2.expr_typ) 
            then
              let (quads3, s_info) = gen_stmt quads2 expr2 in
              let n = s_info.next_stmt in
                (quads2, setStmtInfo n)
            else
              let (quads3, expr2_info) = gen_expr quads2 expr2 in
              let quads4 = backpatch quads3 expr2_info.next_expr (nextLabel ()) in
              let quads5 = genQuad (Q_Assign, expr2_info.place, O_Empty, O_Deref expr1_info.place) quads4 in
              let n = newLabelList () in
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
          let delete_entry = findAuxilEntry "_delete" in
          let quads2 = backpatch quads1 e1_info.next_expr (nextLabel ()) in
          let quads3 = genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads2 in
          let quads4 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry delete_entry) quads3 in
          let s_info = setStmtInfo (newLabelList ()) in
            (quads4, s_info)
        | U_Not -> 
          let (quads1, cond_info) = gen_cond quads expr_node in 
          let next = mergeLabels cond_info.true_lst cond_info.false_lst in
            (quads1, setStmtInfo next)
    end
  | E_Ifthen (expr1, expr2) -> 
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
    let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
    let next = mergeLabels cond_info.false_lst stmt2_info.next_stmt in
      (quads3, setStmtInfo next)
  | E_Ifthenelse (expr1, expr2, expr3) ->
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
    let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
    let l1 = makeLabelList (nextLabel ()) in
    let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
    let quads5 = backpatch quads4 (cond_info.false_lst) (nextLabel ()) in
    let (quads6, stmt3_info) = gen_stmt quads5 expr3 in
    let merged = mergeLabels l1 stmt2_info.next_stmt in 
    let next = mergeLabels stmt3_info.next_stmt merged in
      (quads6, setStmtInfo next)
  | E_While (expr1, expr2) ->
    let q = nextLabel () in
    let (quads1, cond_info) = gen_cond quads expr1 in
    let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
    let (quads3, stmt_info) = gen_stmt quads2 expr2 in
    let quads4 = backpatch quads3 stmt_info.next_stmt q in
    let quads5 = genQuad (Q_Jump, O_Empty, O_Empty, O_Label q) quads4 in
    let next = setStmtInfo (cond_info.false_lst) in
      addLabelTbl q;
      (quads5, next)
  | E_For (id, expr1, cnt, expr2, expr3) ->
    let (relop, op) = match cnt with 
      | To -> (Q_G, Q_Plus)
      | Downto -> (Q_L, Q_Minus) 
    in
    let (quads1, expr1_info) = gen_expr quads expr1 in
    let quads2 = backpatch quads1 expr1_info.next_expr (nextLabel ()) in
    let id_entry = match expr_node.expr_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let obj = O_Entry id_entry in
    let quads3 = genQuad (Q_Assign, expr1_info.place, O_Empty, obj) quads2 in
    let (quads4, expr2_info) = gen_expr quads3 expr2 in
    let quads5 = backpatch quads4 expr2_info.next_expr (nextLabel ()) in
    let condLabel = nextLabel () in
    let next = makeLabelList condLabel in
    let quads6 = genQuad (relop, obj, expr2_info.place, O_Backpatch) quads5 in
    let (quads7, stmt_info) = gen_stmt quads6 expr3 in
    let quads8 = backpatch quads7 stmt_info.next_stmt (nextLabel ()) in
    let quads9 = genQuad (op, obj, O_Int 1, obj) quads8 in
    let quads10 = genQuad (Q_Jump, O_Empty, O_Empty, O_Label condLabel) quads9 in
      addLabelTbl condLabel;
      (quads10, setStmtInfo next)
  | E_Id (id, l) -> 
    let quads1 =
      List.fold_left 
        (fun quads e ->
           if (isUnit e.atom_typ)
           then
             let (quads1, s_info) = gen_atom_stmt quads e in
             let quads2 = backpatch quads1 s_info.next_stmt (nextLabel ()) in
               quads2
           else
             let (quads1, e_info) = gen_atom quads e in
             let quads2 = backpatch quads1 e_info.next_expr (nextLabel ()) in
             let quads3 = genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads2 in
               quads3
        )
        quads l
    in
    let callee_entry = match expr_node.expr_entry with
      | Some e -> e
      | None -> internal "Callee is not a function"
    in
    let quads2 = genQuad (Q_Call, O_Empty, O_Empty, O_Entry callee_entry) quads1 in
    let s_info = setStmtInfo (newLabelList ()) in
      (quads2, s_info)
  | E_Dim _ -> 
    let (quads1, e_info) = gen_expr quads expr_node in
      (quads1, setStmtInfo e_info.next_expr)
  | E_Letin (def, expr) ->
    let delete_quads = ref (newQuadList ()) in
    let quads1 = gen_decl quads def delete_quads in
    let (quads2, stmt_info) = gen_stmt quads1 expr in
    let quads3 = mergeQuads !delete_quads quads2 in
      (quads3, stmt_info)
  | E_New t ->
    let (quads1, expr_info) = gen_expr quads expr_node in
      (quads1, setStmtInfo expr_info.next_expr)
  | E_Match _ | E_Cid _ -> (quads, setStmtInfo (newLabelList ()))  (* TODO dummy return value XXX *)
  | E_Block e -> gen_stmt quads e
  | E_Atom a -> gen_atom_stmt quads a


and gen_atom quads atom_node = match atom_node.atom with
  | A_Num n -> (quads, setExprInfo (O_Int n) (newLabelList ()))
  | A_Dec f -> (quads, setExprInfo (O_Float f) (newLabelList ()))  
  | A_Chr c -> (quads, setExprInfo (O_Char c) (newLabelList ()))
  | A_Str str -> (quads, setExprInfo (O_Str str) (newLabelList ()))
  | A_Bool b -> (quads, setExprInfo (O_Bool b) (newLabelList ()))
  | A_Cid cid -> (quads, setExprInfo O_Empty  (newLabelList ()))  (*Dummy return value*)
  | A_Var v -> 
    let var_entry = match atom_node.atom_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let place = O_Entry var_entry in
      (quads, setExprInfo place (newLabelList ()))
  | A_Par -> internal "Reached unreachable point. Unit in gen_atom.\n"
  | A_Bang a -> 
    let (quads1, expr_info) = gen_atom quads a in   
    let fresh_typ = a.atom_typ in
    let typ = lookup_solved fresh_typ  in
    let temp = newTemp typ (Stack.top offset_stack) in
    let quads2 = backpatch quads1 (expr_info.next_expr) (nextLabel ()) in
    let quads3 = genQuad (Q_Assign, expr_info.place, O_Empty, temp) quads2 in
    let place = O_Deref temp in
      (quads3, setExprInfo place (newLabelList ()))
  | A_Array (id, expr_list) ->
    let fresh_typ = atom_node.atom_typ in
    let typ = lookup_solved fresh_typ  in
    let array_entry = match atom_node.atom_entry with 
      | Some e -> e
      | None -> internal "Expected entry"  
    in
    let (quads1, e_info) = 
      List.fold_left 
        (fun (quads, e_info) e ->
           let (quads1, e_info1) = gen_expr quads e in
           let quads2 = backpatch quads1 e_info1.next_expr (nextLabel ()) in
           let temp = newTemp typ (Stack.top offset_stack) in
           let quads3 = genQuad (Q_Array, e_info.place, e_info1.place, temp) quads2 in
             (quads3, setExprInfo temp (newLabelList ()))
        )
        (quads, setExprInfo (O_Entry array_entry) (newLabelList ())) expr_list
    in
      (quads1, e_info)
  | A_Expr e -> 
    gen_expr quads e

and gen_atom_stmt quads atom_node = match atom_node.atom with
  | A_Num _ | A_Dec _ | A_Str _ 
  | A_Chr _ | A_Bool _ | A_Array _ -> internal "non unit expressions called from atom_stmt"
  | A_Par | A_Var _ -> (quads, setStmtInfo (newLabelList ()))
  | A_Expr e -> gen_stmt quads e 
  | A_Bang a -> 
    let (quads1, expr_info) = gen_atom quads a in   
    let n = expr_info.next_expr in
      (quads1, setStmtInfo n)
  | A_Cid _ -> (quads, setStmtInfo (newLabelList ())) (* dummy return value *)

