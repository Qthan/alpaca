open Types
open Typeinf
open Quads
open QuadLabels

let solved_types = Hashtbl.create 1009 

(* Symbol entry to unsolved type *)
let lookup_type entry =
  match entry with
    | Nothing -> internal "Entry not found\n"
    | Some e ->
        begin
          match e.entry_info with
            | ENTRY_variable v -> v.variable_type
            | ENTRY_function f -> f.function_result
            | ENTRY_parameter p -> p.parameter_type
            | ENTRY_temporary t -> t.temporary_type
            | ENTRY_udt -> T_id (id_name e.entry_id)
            | ENTRY_constructor c -> c.constructor_type
            | ENTRY_none -> internal "Invalid entry %s\n" (id_name e.entry_id)
        end

(* unsolved to solved type *)

let rec lookup_solved tvar ty_table = 
  match tvar with
    | T_Alpha _ ->
        begin 
          match (try Some (Hashtbl.find ty_table tvar) with Not_found -> None) with
            | None -> internal "Failed to locate inferred type\n"
            | Some typ -> 
                 begin
                  match (try Some (checkType typ) with PolymorphicTypes -> None) with
                    | None -> warning "Unused polymorphic type"; raise Exit (* print the type too *)
                    | Some () -> typ
                end
        end
    | T_Ord -> internal "error\n"
    | T_Array (t, d) ->  T_Array (lookup_solved t, d)
    | T_Ref t -> T_Ref (lookup_solved t ty_table)
    | T_Notype -> internal "Invalid type \n"
    | T_Arrow _ -> internal "I used to be a valid type but then I took an arrow to the knee \n"
    | _ -> tvar

let rec gen_program ast subst =
  add_solved_table subst solved_types;
  gen_decl_list ast

and gen_decl_list ast = 
  let outer = genQuad (Q_Unit, O_Fun "outer", O_Empty, O_Empty) (newQuadList ()) in
  List.fold_left gen_decl outer ast

and gen_decl outer stmt = match stmt with
  | S_Let l | S_Rec l -> gen_def_list outer l
  | S_Type l          -> ()

and gen_def_list outer lst = List.fold_left gen_def outer lst

and gen_def quads def_node = match def_node.def with
  | D_Var (lst, expr) -> 
      begin 
        match lst with 
          | (id, _) :: []     ->
              let (quads1, e_info) = gen_expr quads expr in
              let quads2 = backpatch quads1 e_info.next (nextLabel ()) in
              let quads3 = 
                genQuad (Q_Assign, e_info.place, O_Empty, O_Object id) quads2
              in
                quads3
          | (id, _) :: params ->
                let fQuads = newQuadList () in
                let fQuads1 = 
                  genQuad (Q_Unit, O_Fun id, O_Empty, O_Empty) fQuads 
                in
                let (fQuads2, e_info) = gen_expr fQuads1 expr in
                let fQuads3 = backpatch fQuads2 e_info.next (nextLabel ()) in
                let fQuads4 = 
                  genQuad (Q_Assign, e_info.place, O_Empty, O_Res) fQuads3
                in
                let fQuads5 = (Q_Endu, O_Fun id, O_Empty, O_Empty) fQuads4 in 
                  mergeQuads quads fQuads5 
      end
  | D_Mut (_, _)    -> quads
  | D_Array (id, _, lst)  -> 
      let rec gen_array_dims lst quads =
        match lst with
          | [] -> quads
          | dim :: xs ->
              let (quads1, e_info) = gen_expr quads dim in
              let quads2 = genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads1 in
                gen_array_dims xs quads2
      in
      let quads1 = gen_array_dims (List.rev lst) quads in
      let ty = lookup_type def_node.def_entry in
      let solved_ty = lookup_solved ty solved_types in
      let size = sizeOfType ty in  (* Size of an array element *)
      let quads2 = genQuad (Q_Par, O_Size size, O_ByVal, O_Empty) quads1 in
      let dims = arrayDims solved_ty in
      let quads3 = genQuad (Q_Par, O_Dims dims, O_ByVal, O_Empty) quads2 in
      let quads4 = genQuad (Q_Par, O_Obj id, O_Ret, O_Empty) quads3 in
      let quads5 = genQuad (Q_Call, O_Empty, O_Empty, O_Fun "_make_array") in
        quads5

and gen_expr quads expr_node = match expr_node.expr with 
  | E_Binop (expr1, op, expr2) ->
      begin 
        match op with 
          | Plus | Minus | Times | Div | Mod
          | Fplus | Fminus | Ftimes | Fdiv | Power as oper -> 
              let (quads1, e1_info) = gen_expr quads expr1 in
              let quads2 = backpatch quads1 (e1_info.next) (nextLabel ()) in
              let (quads3, e2_info) = gen_expr quads2 expr2 in
              let quads4 = backpatch quads3 (e2_info.next) (nextLabel ()) in
              let typ = expr_node.expr_typ in
              (* let solved = lookup_solved typ solved_types in -- reduntant*) 
              let temp = newTemp typ in
              let quads5 = 
                genQuad (getQuadBop oper, e1_info.place, e2_info.place, temp) quads4
              in
              let e_info = setExprInfo temp (newLabelList ()) in
                (quads5, e_info)
          | Seq | Nseq | Eq | Neq
          | L | Le | G | Ge 
          | And | Or as oper ->
              let (quads1, cond_info) = gen_cond quads expr_node in
              let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
              let temp = newTemp expr_node.expr_typ in
              let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
              let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
              let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
              let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
              let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
                (quads6, e_info)
          | Semicolon -> 
              let (quads1, stmt_info) = gen_stmt quads expr1 in
              let quads2 = backpatch quads1 stmt_info.next (nextLabel ()) in
              let (quads3, expr_info) = gen_expr quads2 expr2 in
                (quads3, expr_info)
          | Assign -> internal "Assign is not an expression\n"
      end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Plus | U_Minus | U_Fplus | U_Fminus as oper ->
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 e1_info.next (nextLabel ()) in
          let typ = expr_node.expr_typ in
          let temp = newTemp typ in
          let quads3 = genQuad (getQuadUnop oper, e1_info.place, O_Empty, temp) quads2 in
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
          let temp = newTemp expr_node.expr_typ in
          let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
          let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
          let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
          let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
          let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
            (quads6, e_info)
    end 
  | E_Id (id, l) -> 
      let quads1 =
        List.foldl 
          (fun e quads ->
             let (quads1, e_info) = gen_expr e quads in
             let quads2 = backpatch quads1 e_info.next (nextLabel ()) in
             let quads3 =
               if (checkUnit e.expr_typ) 
               then genQuad (Q_Par, e_info.place, O_ByVal, O_Empty) quads2
               else quads2
             in
               quads3
          )
          quads l
      in
      let fresh_typ = expr_node.expr_typ in
      let typ = lookup_solved fresh_typ solved_types in
      let temp = newTemp typ in
      let quads2 = genQuad (Q_Par, temp, O_Ret, O_Empty) quads1 in
      let quads3 = genQuad (Q_Call, O_Empty, O_Empty, O_Fun id) in
      let e_info = setExprInfo temp (newLabelList ()) in
        (quads3, e_info)


  | E_Ifthen (expr1, expr2) ->
  | E_Ifthenelse (expr1, expr2, expr3) ->
  | E_Atom a -> gen_atom quads a
            
and gen_cond quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
      begin
        match op with 
          | Seq | Nseq | Eq | Neq
          | L | Le | G | Ge as oper ->
              let (quads1, e1_info) = gen_expr quads expr1 in
              let quads2 = backpatch quads1 e1_info.nextLabel (nextLabel ()) in
              let (quads3, e2_info) = gen_expr quads2 expr2 in
              let quads4 = backpatch quads3 e2_info.nextLabel (nextLabel ()) in
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
              let quads2 = backpatch quads1 stmt_info.next (nextLabel ()) in
              let (quads3, cond_info) = gen_cond quads2 expr2 in
                (quads3, cond_info)
          | Assign -> internal "unreachable: Assign in gen_cond"
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
    end
  | E_Atom a ->
      begin
      match a with
        | A_Bool b ->
            let cond_lst = makeLabelList (nextLabel ()) in
            let quads1 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads in
              (match b with
                | true -> (quads1, setCondInfo cond_lst (newLabelList ()))
                | false -> (quads1, setCondInfo (newLabelList ()) cond_lst))
        | A_var id -> 
            let true_lst = makeLabelList (nextLabel ()) in
            let quads1 = genQuad (Q_Ifb, (O_Obj (id, T_Bool)), O_Empty, O_Backpatch) quads in
            let false_lst = makeLabelList (nextLabel ()) in
            let quads2 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads1 in
            let cond_info = setCondInfo true_lst false_lst in
              (quads2, cond_info)
        | A_Bang banged -> 
            let (quads1, expr_info) = gen_atom a quads in   
            let quads2 = backpatch quads1 (expr_info.next) (nextLabel ()) in
            let true_lst = makeLabelList (nextLabel ()) in
            let quads3 = genQuad (Q_Ifb, expr_info.place, O_Empty, O_Backpatch) quads2 in
            let false_lst = makeLabelList (nextLabel ()) in
            let quads4 = genQuad(Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
            let cond_info = setCondInfo true_lst false_lst in
              (quads4, cond_info)
        | A_Expr e -> gen_cond quads e
      end

and gen_stmt quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
      begin 
        match op with 
          | Plus | Minus | Times | Div | Mod
          | Fplus | Fminus | Ftimes | Fdiv | Power as oper -> 
              let (quads1, e1_info) = gen_expr quads expr_node in
              let s_info = setStmtInfo (e1_info.next) in 
                (quads1, s_info)
          | Seq | Nseq | Eq | Neq
          | L | Le | G | Ge 
          | And | Or as oper ->
              let (quads1, cond_info) = gen_cond quads expr_node in
              let n = mergeLabels cond_info.true_lst cond_info.false_lst in
              let s_info = setStmtInfo n in
                (quads1, s_info)
          | Semicolon     -> 
              let (quads1, stmt_info) = gen_stmt quads expr1 in
              let quads2 = backpatch quads1 stmt_info.next (nextLabel ()) in
              let (quads3, stmt2_info) = gen_stmt quads2 expr2 in
                (quads3, stmt2_info)
          | Assign        -> 
              let (quads1, expr1_info) = gen_expr quads expr1 in
              let quads2 = backpatch quads1 expr1_info.next (nextLabel ()) in
              let (quads3, expr2_info) = gen_expr quads2 expr2 in
              let quads4 = backpatch quads3 expr2_info.next (nextLabel ()) in
              let quads5 = genQuad (Q_Assign, expr2_info.place, O_Empty, O_Deref expr1_info.place) quads4 in
              let n = newLabelList () in
                (quads5, setStmtInfo n)
      end
  | E_Unop (op, expr1) ->
    begin
      match op with
        | U_Plus | U_Minus | U_Fplus | U_Fminus as oper ->
            let (quads1, e_info) = gen_expr expr_node in
            (quads1, setStmtInfo e_info.next)
        | U_Del -> 
          let (quads1, e1_info) = gen_expr quads expr1 in
          let quads2 = backpatch quads1 e1_info.next (nextLabel ()) in
          let quads3 = genQuad (Q_Par, e1_info.place, O_ByVal, O_Empty) quads2 in
          let quads4 = genQuad (Q_Call, O_Empty, O_Empty, O_Fun "_delete") quads3 in
          let s_info = setStmtInfo (newLabelList ()) in
            (quads4, s_info)
        | U_Not -> 
          let (quads1, cond_info) = gen_cond quads expr_node in 
          let next = mergeLabels cond_info.true_lst cond_info.false_lst in
            (quads1, setStmtInfo next)
    end

and gen_atom quads atom_node = match atom_node.atom with
  | A_Num n -> (quads, setExprInfo (O_Int n) (newLabelList ()))
  | A_Dec f -> (quads, setExprInfo (O_Float f) (newLabelList ()))  
  | A_Chr c -> (quads, setExprInfo (O_Char c) (newLabelList ()))
  | A_Str str -> (quads, setExprInfo (O_String str) (newLabelList ()))
  | A_Bool b -> (quads, setExprInfo (O_Bool b) (newLabelList ()))
  | A_Cid cid -> ([], setExprInfo O_Empty  (newLabelList ()))  (*Dummy return value*)
  | A_Var v -> 
      let fresh_typ = lookup_type atom_node.atom_entry in
      let typ = lookup_solved fresh_typ solved_types in
      let place = O_Obj (v, typ) in
        (quads, setExprInfo place (newLabelList ()))
  | A_Par -> internal "Reached unreachable point. Unit in gen_atom.\n"
  | A_Bang a -> 
      let (quads1, expr_info) = gen_atom a quads in   
      let fresh_typ = atom.atom_typ in
      let typ = lookup_solved fresh_typ solved_types in
      let temp = newTemp typ in
      let quads2 = backpatch quads1 (expr_info.next) (nextLabel ()) in
      let quads3 = genQuad (Q_Assign, expr_info.place, O_Empty, temp) quads2 in
      let place = O_Deref temp in
        (quads3, setExprInfo place (newLabelList ()))
  | A_Array (id, expr_list) ->
      let fresh_typ = atom.atom_typ in
      let typ = lookup_solved fresh_typ solved_types in
      let fresh_arr_typ = lookup_type (atom_node.atom_entry) in
      let arr_typ = lookup_solved fresh_arr_typ solved_types in
      let (quads1, e_info) = 
        List.foldl 
          (fun e (quads, e_info) ->
             let (quads1, e_info1) = gen_expr e quads in
             let quads2 = backpatch quads1 e_info1.next (nextLabel ()) in
             let temp = newTemp typ in
             let quads3 = genQuad (Q_Array, e_info.place, e_info1.place, temp) quads2 in
               (quads3, setExprInfo temp (newLabelList ()))
          )
          (quads, setExprInfo (Obj (id, arr_typ)) (newLabelList ())) expr_list
      in
        (quads1, e_info)
  | A_expr e -> 
      gen_expr quads e

            

e1; e2; ....; en;
 gen_decl e1---en-1;
 gen_epxr en;
 
 kai genika tha prepei na vlepoume pote mia sunarthsh h variable exi tupo 
 epistrofhs unit kai na to xeirizomaste diaforetika (px na kaloume walk_stmt)
 


1:  unit, fun[_outer; 0; 0; 4; 0; 0], -, -
2:	par, 10, V, -
3:	par, temp[$1; float ref; -4], RET, -
4:	call, -, -, fun[_new; 0; 2; 0; 0; 1]
5:	:=, temp[$1; float ref; -4], -, obj[b; float ref; -2; 0; 0]
6:	par, obj[b; float ref; -2; 0; 0], V, -
7:	call, -, -, fun[_delete; 0; 2; 0; 0; 0]
8:	endu, fun[_outer; 0; 0; 4; 0; 0], -, -



let a = -4

1:  unit, fun[_outer; 0; 0; 4; 0; 0], -, -
2:	-, 4, -, temp[$1; int; -4]
3:	:=, temp[$1; int; -4], -, obj[a; int; -2; 0; 0]
4:	endu, fun[_outer; 0; 0; 4; 0; 0], -, -


1:  unit, _outer, -, -
2:	par, 20, V, -
3:	par, 2, V, -
4:	par, 1, V, -
5:	par, p, RET, -
6:	call, -, -, _make_array
7:	array, p, 1, $1
8:	:=, 4, -, [$1]
9:	par, p, V, -
10:	call, -, -, _delete
11:	endu, _outer, -, -

let a = (3 < 5) && (5 > 1)
1:  unit, fun[_outer; 0; 0; 2; 0; 0], -, -
2:	<, 3, 5, 4
3:	jump, -, -, 8
4:	>, 5, 1, 6
5:	jump, -, -, 8
6:	:=, true, -, temp[$1; bool; -2]
7:	jump, -, -, 9
8:	:=, false, -, temp[$1; bool; -2]
9:	:=, temp[$1; bool; -2], -, obj[a; bool; -1; 0; 0]
10:	endu, fun[_outer; 0; 0; 2; 0; 0], -, -

              let (quads1, cond_info) = gen_cond quads expr_node in
              let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
              let temp = newTemp expr_node.expr_typ in
              let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
              let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
              let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
              let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
              let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
                (quads6, e_info)

cond1: t = [2], f = [3]
comd2: t = [4], f = [5]
condand = t = [4], f = [3, 5]

2:  <, 3, 5, 4
3:	jump, -, -, 8

4:  >, 5, 1, 6
5:	jump, -, -, 8

6: :=, true, -, $1
7: Jump, -, -, *

8: := false, -, $1





