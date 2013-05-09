open Error
open Types
open Identifier
open Printf
open Pretty_print
open Format

(* XXX VOTE : Handling errors with exceptions *)
exception Unify of typ * typ

(* Function for type inference debugging *)

let debug_typeinf = false

let print_constraints lst = 
  let rec pp_constraints ppf solved = 
    let pp_tuple ppf (t1, t2) =
      fprintf ppf "(%a, %a)\n" pretty_typ t1 pretty_typ t2 
    in
    match solved with
    | [] -> ()
    | x :: [] -> fprintf ppf "%a" pp_tuple x
    | x :: xs -> fprintf ppf "%a %a" pp_tuple x pp_constraints xs
  in
  printf "%a" pp_constraints lst

(* Save inferred types on a hashtable for fast look-ups *)

let add_solved_table solved tbl = 
  List.iter (fun (tvar, typ) -> Hashtbl.add tbl tvar typ) solved 

(* Type inference functions *)

let fresh =
  let k = ref 0 in
  fun () -> incr k; T_Alpha !k

let freshDim =
  let k = ref 0 in
  fun () -> incr k; D_Alpha !k

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
  | T_Alpha _, T_Ref typ1 -> T_Ref (singleSub alpha t2 typ1)
  | T_Alpha _, T_Array (typ1,n)-> T_Array ((singleSub alpha t2 typ1),n)
  | T_Alpha _, _ -> typ
  | _, _ -> failwith "must be alpha"

let subc alpha tau c =
  let walk (tau1, tau2) = (singleSub alpha tau tau1, singleSub alpha tau tau2) in
  List.map walk c

let rec singleSubDim alpha d dim1 = match alpha, dim1 with 
  | D_Alpha a, D_Alpha b when a = b -> d
  | D_Alpha _, D_Alpha b -> D_Alpha b
  | D_Alpha _, _ -> dim1
  | _, _ -> failwith "Must be D_Alpha"

let subDim alpha d lst =
  let walk (dim1, dim2) = (singleSubDim alpha d dim1, singleSubDim alpha d dim2) in
  List.map walk lst

let rec singleSubArray alpha d tau = match alpha, tau with
  | alpha, T_Array (tau, dim1) -> T_Array (tau, singleSubDim alpha d dim1)
  | _, tau -> tau

let subArray alpha d lst =
  let walk (dim1, dim2) = (singleSubArray alpha d dim1, singleSubArray alpha d dim2) in
  List.map walk lst

let equalsType tau1 tau2  = match tau1, tau2 with
  | T_Ord, tau | tau, T_Ord -> (tau = T_Int) || (tau = T_Float) || (tau = T_Char)
  | tau1, tau2 -> tau1 = tau2

let unify c =
  let rec unifyDims dims acc = match dims with
    | [] -> acc
    | (D_Int a, D_Int b) :: lst when a = b -> unifyDims lst acc 
    | (D_Alpha alpha, dim2) :: lst -> unifyDims (subDim (D_Alpha alpha) dim2 lst) (subArray (D_Alpha alpha) dim2 acc)  
    | (dim1, D_Alpha alpha) :: lst -> unifyDims (subDim (D_Alpha alpha) dim1 lst) (subArray (D_Alpha alpha) dim1 acc)
    | (dim1, dim2) :: lst -> printf "Could not match dim %a with dim %a \n" pretty_dim dim1 pretty_dim dim2; raise Exit
  in 
  let rec unifyOrd ord acc = match ord with
    | [] -> acc 
    | (tau1, tau2) :: c when (equalsType tau1 tau2) -> unifyOrd c acc
    | (T_Alpha alpha, T_Ord) :: c | (T_Ord, T_Alpha alpha) :: c -> 
      unifyOrd (subc (T_Alpha alpha) T_Ord c) ((T_Alpha alpha, T_Ord) :: (subc (T_Alpha alpha) T_Ord acc))
    | (typ1, typ2) :: lst -> printf "Could not match type %a with type %a \n" pretty_typ typ1 pretty_typ typ2; raise Exit  
  in
  let rec unifyAux c ord dims acc = match c with
    | [] -> 
      let acc' = unifyOrd ord acc in 
      let solved = unifyDims dims acc' in
      if (debug_typeinf) then print_constraints solved;
      solved
    | (tau1, tau2) :: c when equalsType tau1 tau2 -> 
      unifyAux c ord dims acc
    | (T_Ref tau1, T_Ref tau2) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord dims acc
    | (T_Array (tau1, dim1), T_Array (tau2, dim2)) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord ((dim1, dim2) :: dims) acc
    | (tau1, tau2) :: c when equalsType tau1 tau2 -> 
      unifyAux c ord dims acc
    | (T_Ord, tau2) :: c -> 
      unifyAux c ((T_Ord, tau2) :: ord) dims acc
    | (tau1, T_Ord) :: c -> 
      unifyAux c ((tau1, T_Ord) :: ord) dims acc
    | (T_Alpha alpha, tau2) :: c when notIn (T_Alpha alpha) tau2 ->
      unifyAux (subc (T_Alpha alpha) tau2 c) (subc (T_Alpha alpha) tau2 ord) dims ((T_Alpha alpha, tau2) :: (subc (T_Alpha alpha) tau2 acc))
    | (tau1, T_Alpha alpha) :: c when notIn (T_Alpha alpha) tau1 ->
      unifyAux (subc (T_Alpha alpha) tau1 c) (subc (T_Alpha alpha) tau1 ord) dims ((T_Alpha alpha, tau1) :: (subc (T_Alpha alpha) tau1 acc))
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
      unifyAux ((tau11, tau21) :: (tau12, tau22) :: c) ord dims acc
    | (typ1, typ2) :: lst -> printf "Could not match type %a with type %a \n" pretty_typ typ1  pretty_typ typ2; raise Exit
  in
  unifyAux c [] [] []


