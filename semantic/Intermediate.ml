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
  gen_stmt_list ast

and gen_stmt_list ast = 
  let outer = genQuad (Q_Unit, O_Fun "outer", O_Empty, O_Empty) (newQuadList ()) in
  List.fold_left gen_stmt outer ast

and gen_stmt outer stmt = match stmt with
  | S_Let l | S_Rec l -> gen_def_list outer l
  | S_Type l          -> ()

and gen_def_list outer lst = List.fold_left gen_def outer lst

and gen_def quads def_node = match def_node.def with
  | D_Var (lst, expr) -> 
      begin 
        match lst with 
          | (id, _) :: []     ->
              let (quads1, e_info) = gen_expr quads expr in
              let quads2 = 
                genQuad (Q_Assign, e_info.place, O_Empty, O_Object id) quads1
              in
                quads2
          | (id, _) :: params ->
                let fQuads = newQuadList () in
                let fQuads1 = 
                  genQuad (Q_Unit, O_Fun id, O_Empty, O_Empty) fQuads 
                in
                let (fQuads2, e_info) = gen_expr fQuads1 expr in
                let fQuads3 = 
                  genQuad (Q_Assign, e_info.place, O_Empty, O_Res) fQuads2
                in
                let fQuads4 = (Q_Endu, O_Fun id, O_Empty, O_Empty) fQuads3 in 
                  mergeQuads quads fQuads4 
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
              let (quads2, e2_info) = gen_expr quads1 expr2 in
              let typ = expr_node.expr_typ in
              let solved = lookup_solved typ solved_types in
              let temp = newTemp solved in
              let quads3 = 
                genQuad (getQuadOp oper, e1_info.place, e2_info.place, temp) quads2
              in
              let e_info = setExprInfo (Some temp) (newLabelList ()) in
                (quads3, e_info)
          | Seq | Nseq | Eq | Neq
          | L | Le | G | Ge as oper ->
              let (quads1, cond_info) = gen_cond quads expr_node in
              let quads2 = backpatch quads1 cond_info.true_lst (nextLabel ()) in
              let temp = newTemp expr_node.expr_typ in
              let quads3 = genQuad (Q_Assign, O_Bool true, O_Empty, temp) quads2 in
              let e_info = setExprInfo temp (makeLabelList (nextLabel ())) in
              let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
              let quads5 = backpatch quads4 cond_info.false_lst (nextLabel ()) in
              let quads6 = genQuad (Q_Assign, O_Bool false, O_Empty, temp) quads5 in
                (quads6, e_info)
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
                (T_Ref expr2.expr_typ, expr1.expr_typ) :: constraints1 @ constraints2
      end

and gen_cond quads expr_node = match expr_node.expr with
  | E_Binop (expr1, op, expr2) ->
      begin
        match op with 
          | Seq | Nseq | Eq | Neq
          | L | Le | G | Ge as oper ->
              let (quads1, e1_info) = gen_expr expr1 quads in
              let (quads2, e2_info) = gen_expr expr2 quads1 in
              let t = makeLabelList (nextLabel ()) in
              let quads3 = 
                genQuad (getQuadBop oper, e1_info.place, e2_info.place, O_Backpatch) quads2
              in
              let f = makeLabelList (nextLabel ()) in
              let quads4 = genQuad (Q_Jump, O_Empty, O_Empty, O_Backpatch) quads3 in
              let cond_info = setCondInfo t f in
                (quads4, cond_info)
      end

1:	unit, _outer, -, -
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
                           
