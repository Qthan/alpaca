open Error
open Types
open Identifier
open Symbol

let fresh =
  let k = ref 0 in
    fun () -> incr k; T_Alpha !k

let refresh ty = 
  match ty with
    | T_Notype -> fresh ()
    | _        -> ty

let rec notIn alpha typ = match typ with
  | T_Alpha n -> alpha != (T_Alpha n)
  | T_Arrow (t1, t2) -> (notIn alpha t1) && (notIn alpha t2)
  | T_Array(a,n) -> a != alpha 
  | T_Ref tref -> tref != alpha
  | T_Int  | T_Char  | T_Str | T_Unit| T_Id _ 
  | T_Ord | T_Bool | T_Float | T_Notype -> true

let rec singleSub alpha t2 typ = match alpha, typ with
  | T_Alpha a, T_Alpha n when a = n -> t2
  | T_Alpha _, T_Alpha _ -> typ
  | T_Alpha _, T_Arrow (typ1,typ2) -> T_Arrow ((singleSub alpha t2 typ1),(singleSub alpha t2 typ2))
  | T_Alpha _, T_Ref typ1-> T_Ref (singleSub alpha t2 typ1)
  | T_Alpha _, T_Array (typ1,n)-> T_Array ((singleSub alpha t2 typ1),n)
  | T_Alpha _, _ -> typ
  | _, _ -> failwith "must be alpha"

let rec subc alpha tau c =
  let walk (tau1, tau2) = (singleSub alpha tau tau1, singleSub alpha tau tau2) in
    List.map walk c

let equalsType tau1 tau2  = match tau1, tau2 with
  | T_Ord, tau | tau, T_Ord -> (tau = T_Int) || (tau = T_Float) || (tau = T_Char)
  | tau1, tau2 -> tau1 = tau2

let unify c =
  let rec unifyOrd ord acc = match ord with
    | [] -> acc 
    | (tau1, tau2) :: c when (equalsType tau1 tau2) -> unifyOrd c acc
    | (T_Alpha alpha, T_Ord) :: c | (T_Ord, T_Alpha alpha) :: c -> 
        unifyOrd (subc (T_Alpha alpha) T_Ord c) ((T_Alpha alpha, T_Ord) :: (subc (T_Alpha alpha) T_Ord acc))
    | _ -> failwith "ERROR !!! BOO !!!"
  in
  let rec unifyAux c ord acc = match c with
    | [] -> (unifyOrd ord acc)
    | (tau1, tau2) :: c when equalsType tau1 tau2 -> unifyAux c ord acc
    | (T_Ord, tau2) :: c -> unifyAux c ((T_Ord, tau2) :: ord) acc
    | (tau1, T_Ord) :: c -> unifyAux c ((tau1, T_Ord) :: ord) acc
    | (T_Alpha alpha, tau2) :: c when notIn (T_Alpha alpha) tau2 ->
        unifyAux (subc (T_Alpha alpha) tau2 c) (subc (T_Alpha alpha) tau2 ord) ((T_Alpha alpha, tau2)::(subc (T_Alpha alpha) tau2 acc))
    | (tau1, T_Alpha alpha) :: c when notIn (T_Alpha alpha) tau1 ->
        unifyAux (subc (T_Alpha alpha) tau1 c) (subc (T_Alpha alpha) tau1 ord) ((T_Alpha alpha, tau1)::(subc (T_Alpha alpha) tau1 acc))
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
        unifyAux ((tau11, tau21) :: (tau12, tau22) :: c) ord acc
    | _ -> failwith "ERROR !!! BOO !!!"
  in
    unifyAux c [] []


(* Old Unify
 
let unify c =
  let rec unifyAux c acc = match c with
    | [] -> acc
    | (tau1, tau2) :: c when tau1 = tau2 -> unifyAux c acc
    | (T_Alpha alpha, tau2) :: c when notIn (T_Alpha alpha) tau2 ->
        unifyAux (subc (T_Alpha alpha) tau2 c) ((T_Alpha alpha, tau2)::(subc (T_Alpha alpha) tau2 acc))
    | (tau1, T_Alpha alpha) :: c when notIn (T_Alpha alpha) tau1 ->
        unifyAux (subc (T_Alpha alpha) tau1 c) ((T_Alpha alpha, tau1)::(subc (T_Alpha alpha) tau1 acc))
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
        unifyAux ((tau11, tau21) :: (tau12, tau22) :: c) acc
    | _ -> failwith "ERROR !!! BOO !!!"
  in
    unifyAux c []
 *)

let updateSymbol func_header solved_types = match func_header with
  | [] -> failwith "func_header cannot be empty\n";
  | (id, _)::_ -> 
      let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        begin
          match p.entry_info with 
            | ENTRY_function f -> 
                let f_typ = List.assoc f.function_result solved_types in
                  f.function_result <- f_typ;
                  List.iter (fun param_entry -> match param_entry.entry_info with
                               | ENTRY_parameter param ->
                                  begin
                                    match (try ( Some ( List.assoc param.parameter_type solved_types) ) with Not_found -> None) with
                                                | None -> ()
                                                | Some p_typ -> param.parameter_type <- p_typ 
                                  end
                               | _ -> failwith "Parameter must be a parameter\n"
                  ) f.function_paramlist
            | ENTRY_variable v -> 
                    begin  
                       match (try ( Some ( List.assoc v.variable_type solved_types) ) with Not_found -> None) with
                        | None -> ()
                        | Some v_typ ->  v.variable_type <- v_typ
                    end
            | _ -> failwith "Must be variable or function\n"
        end

let rec updateSymbolRec func_to_change solved_types = match func_to_change with
  | [] -> ()
  | (D_Var (fh, _))::t -> updateSymbol fh solved_types;
                          updateSymbolRec t solved_types
  | _ -> failwith "Must be D_Var\n"
