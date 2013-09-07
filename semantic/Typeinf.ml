open Error
open Types
open Identifier
open Printf
open Pretty_print
open Format

(* Handling type errors with exceptions *)
exception TypeError of string * typ
exception UnifyError of typ * typ
exception DimError of dim * dim 

(* Function for type inference debugging *)

let debug_typeinf = true

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

let solved_types = Hashtbl.create 1009 

let add_solved_table solved = 
  List.iter (fun (tvar, typ) -> Hashtbl.add solved_types tvar typ) solved 

(* unsolved to solved type *)

let rec lookup_solved tvar = 
  match tvar with
    | T_Alpha _ ->
      begin 
        match (try Some (Hashtbl.find solved_types tvar) with Not_found -> None) with
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
    | T_Ref t -> T_Ref (lookup_solved t)
    | T_Notype -> internal "Invalid type \n"
    | T_Arrow (t1, t2) -> T_Arrow(lookup_solved t1, lookup_solved t2) 
    | _ -> tvar

(* Type inference functions *)

let fresh =                                             (* Return a fresh type variable *)
  let k = ref 0 in
    fun () -> incr k; T_Alpha !k

let freshDim =                                          (* Return a fresh dimention variable *)
  let k = ref 0 in
    fun () -> incr k; D_Alpha !k

let refresh ty =                                        (* Return a fresh type variable for undefined types *) 
  match ty with
    | T_Notype -> fresh ()
    | _        -> ty

let rec notIn alpha typ = match typ with                (* check whether a type occurs in an other *)
  | T_Alpha n -> alpha != (T_Alpha n)
  | T_Arrow (t1, t2) -> (notIn alpha t1) && (notIn alpha t2)
  | T_Array(a,n) -> a != alpha 
  | T_Ref tref -> tref != alpha
  | T_Int  | T_Char  | T_Str | T_Unit| T_Id _ 
  | T_Ord | T_Bool | T_Float | T_Nofun -> true
  | T_Notype -> internal "Found indefined type in type inference (T_Notype)"

let rec singleSub alpha t typ = match alpha, typ with   (* substitute alpha with t in typ *)
  | T_Alpha a, T_Alpha n when a = n -> t
  | T_Alpha _, T_Alpha _ -> typ
  | T_Alpha _, T_Arrow (typ1,typ2) -> T_Arrow ((singleSub alpha t typ1),(singleSub alpha t typ2))
  | T_Alpha _, T_Ref typ1 -> T_Ref (singleSub alpha t typ1)
  | T_Alpha _, T_Array (typ1,n)-> T_Array ((singleSub alpha t typ1),n)
  | T_Alpha _, _ -> typ
  | _, _ -> internal "Cannot substitute a non fresh type"

let subc alpha tau c =                                  (* substitute alpha with tau in a tuple list c *)
  let walk (tau1, tau2) = (singleSub alpha tau tau1, singleSub alpha tau tau2) in
    List.map walk c

let subl alpha tau l =                                  (* substitute alpha with tau in a simple list *)
  List.map (singleSub alpha tau) l

let rec singleSubDim alpha d dim1 = match alpha, dim1 with 
  | D_Alpha a, D_Alpha b when a = b -> d
  | D_Alpha _, D_Alpha b -> D_Alpha b
  | D_Alpha _, _ -> dim1
  | _, _ -> internal "Cannot substitute non fresh dimention type"

let subDim alpha d lst =
  let walk (dim1, dim2) = (singleSubDim alpha d dim1, singleSubDim alpha d dim2) in
    List.map walk lst

let rec singleSubArray alpha d tau = match alpha, tau with
  | alpha, T_Array (tau, dim1) -> T_Array (tau, singleSubDim alpha d dim1)
  | _, tau -> tau

let subArray alpha d lst =
  let walk (dim1, dim2) = (singleSubArray alpha d dim1, singleSubArray alpha d dim2) in
    List.map walk lst

(* let equalsType tau1 tau2  = match tau1, tau2 with       
   | T_Ord, tau | tau, T_Ord -> (tau = T_Int) || (tau = T_Float) || (tau = T_Char)
   | T_Nofun, tau | tau, T_Nofun -> 
      begin 
        match tau with
          | T_Arrow (_,_) -> false 
          |  
   | tau1, tau2 -> tau1 = tau2 *)

let unify c =
  let rec unifyDims dims acc = match dims with
    | [] -> acc
    | (D_Int a, D_Int b) :: lst when a = b -> unifyDims lst acc 
    | (D_Alpha alpha, dim2) :: lst -> unifyDims (subDim (D_Alpha alpha) dim2 lst) (subArray (D_Alpha alpha) dim2 acc)  
    | (dim1, D_Alpha alpha) :: lst -> unifyDims (subDim (D_Alpha alpha) dim1 lst) (subArray (D_Alpha alpha) dim1 acc)
    | (dim1, dim2) :: lst -> printf "Could not match dim %a with dim %a \n" pretty_dim dim1 pretty_dim dim2; raise Exit
  in 
  (* ** Old unifyOrd **
     let rec unifyOrd ord acc = match ord with
     | [] -> acc 
     | (tau1, tau2) :: c when (equalsType tau1 tau2) -> unifyOrd c acc
     | (T_Alpha alpha, T_Ord) :: c | (T_Ord, T_Alpha alpha) :: c -> 
      unifyOrd (subc (T_Alpha alpha) T_Ord c) ((T_Alpha alpha, T_Ord) :: (subc (T_Alpha alpha) T_Ord acc))
     | (typ1, typ2) :: lst -> printf "Could not match type %a with type %a \n" pretty_typ typ1 pretty_typ typ2; raise Exit  
     in *)
  let rec unifyOrd ord = match ord with
    | [] -> ()
    | T_Int :: c | T_Float :: c | T_Bool :: c | (T_Alpha _) :: c ->
      unifyOrd c
    | typ :: _ -> 
      raise (TypeError ("Type does not support ordering", typ))
  in
  let rec unifyNofun nofun = match nofun with
    | [] -> ()
    | (T_Arrow (a, b)) :: c -> 
      raise (TypeError ("Cannot return function type", T_Arrow(a, b)))
    | _ :: c -> 
      unifyNofun c
  in
  let rec unifyAux c ord dims nofun acc = match c with
    | [] -> 
      unifyOrd ord;
      unifyNofun nofun;
      unifyDims dims acc
    | (tau1, tau2) :: c when tau1 = tau2 -> 
      unifyAux c ord dims nofun acc
    | (T_Nofun, tau1) :: c | (tau1, T_Nofun) :: c ->
      unifyAux c ord dims (tau1 :: nofun) acc 
    | (T_Ord, tau1) :: c | (tau1, T_Ord) :: c -> 
      unifyAux c (tau1 :: ord) dims nofun acc
    | (T_Ref tau1, T_Ref tau2) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord dims nofun acc
    | (T_Array (tau1, dim1), T_Array (tau2, dim2)) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord ((dim1, dim2) :: dims) nofun acc
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
      unifyAux ((tau11, tau21) :: (tau12, tau22) :: c) ord dims nofun acc
    | (T_Alpha alpha, tau1) :: c 
    | (tau1, T_Alpha alpha) :: c when notIn (T_Alpha alpha) tau1 -> (* When applies to both patterns *)
      let sub_tlist = subc (T_Alpha alpha) tau1 in
      let sub_list = subl (T_Alpha alpha) tau1 in
        unifyAux (sub_tlist c) (sub_list ord) dims (sub_list nofun) ((T_Alpha alpha, tau1) :: (sub_tlist acc))
    | (typ1, typ2) :: lst -> raise (UnifyError (typ1,  typ2))
  in
  let solved = unifyAux c [] [] [] [] in
    if (debug_typeinf) then print_constraints solved;
    add_solved_table solved;
    solved   (*TODO : to be deleted *)
