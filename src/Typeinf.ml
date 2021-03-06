open Error
open Types
open Identifier
open Printf
open Pretty_print
open Format

(* Handling type errors with exceptions *)
exception TypeError of string * typ
exception UnifyError of typ * typ
exception DimSizeError of int * int 
exception DimAccesError of int * int 
exception UnsolvedTyVar of typ
exception UnsolvedDimVar of dim
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

let solved_types = Hashtbl.create 1009 

let add_solved_table solved = 
  List.iter (fun (tvar, typ) -> Hashtbl.add solved_types tvar typ) solved 

(* unsolved to solved type *)

let rec lookup_solved tvar = 
  match tvar with
    | T_Alpha _ ->
      begin
        match (try Some (Hashtbl.find solved_types tvar) 
               with Not_found -> None) with
          | None -> internal "Failed to locate inferred type\n"
          | Some typ -> 
            begin
              match (try Some (checkType typ) 
                     with PolymorphicTypes -> None) with
                | None -> 
                  raise (UnsolvedTyVar tvar) 
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

(* Return a fresh type variable *)
let fresh =   
  let k = ref 0 in
    fun () -> incr k; T_Alpha !k

let dim_size i = D_DimSize i

let freshDim = 
  let k = ref 0 in 
    fun () -> incr k; D_Alpha !k


(* List for collecting constrains arising from dim i a *)
let dimConstraints = ref []

(* Add a constraint to the above list *)
let dimsGeq dims i = 
  dimConstraints := (dims, D_DimSize i) :: !dimConstraints

(* Sumbstitute dimension variable alpha with d in the dimension type dim1 *)                
let rec singleSubDim alpha d dim1 = match alpha, dim1 with 
  | D_Alpha a, D_Alpha b when a = b -> d
  | D_Alpha _, D_Alpha b -> D_Alpha b
  | D_Alpha _, _ -> dim1
  | _, _ -> internal "Cannot substitute non fresh dimension type"

(* Sumbstitute dimension variable alpha with d in the dimensions list lst *)                
let subDim alpha d lst =
  let walk (dim1, dim2) = 
    (singleSubDim alpha d dim1, singleSubDim alpha d dim2) 
  in
    List.map walk lst

(* Sumbstitute dimension variable alpha with d in the type tau *)                
let rec singleSubArray alpha d tau = match alpha, tau with
  | alpha, T_Array (tau, dim1) -> T_Array (tau, singleSubDim alpha d dim1)
  | _, tau -> tau

(* Sumbstitute dimension variable alpha with d in the types list lst *)                
let subArray alpha d lst =
  let walk (dim1, dim2) = 
    (singleSubArray alpha d dim1, singleSubArray alpha d dim2) 
  in
    List.map walk lst

(* Return a fresh type variable for undefined types *) 
let refresh ty =     
  match ty with
    | T_Notype -> fresh ()
    | _        -> ty

(* check whether a type occurs in an other *)
let rec notIn alpha typ = match typ with                
  | T_Alpha n -> alpha != (T_Alpha n)
  | T_Arrow (t1, t2) -> (notIn alpha t1) && (notIn alpha t2)
  | T_Array(a,n) -> a != alpha 
  | T_Ref tref -> tref != alpha
  | T_Int  | T_Char  | T_Unit| T_Id _ 
  | T_Ord | T_Bool | T_Float | T_Nofun | T_Noarr-> true
  | T_Notype -> internal "Found undefined type in type inference (T_Notype)"

(* substitute alpha with t in typ *)
let rec singleSub alpha t typ = match alpha, typ with
  | T_Alpha a, T_Alpha n when a = n -> t
  | T_Alpha _, T_Alpha _ -> typ
  | T_Alpha _, T_Arrow (typ1, typ2) -> 
    T_Arrow ((singleSub alpha t typ1),(singleSub alpha t typ2))
  | T_Alpha _, T_Ref typ1 -> T_Ref (singleSub alpha t typ1)
  | T_Alpha _, T_Array (typ1,n)-> T_Array ((singleSub alpha t typ1), n)
  | T_Alpha _, _ -> typ
  | _, _ -> internal "Cannot substitute a non fresh type"

(* substitute alpha with tau in a tuple list c *)
let subc alpha tau c =  
  let walk (tau1, tau2) = 
    (singleSub alpha tau tau1, singleSub alpha tau tau2)
  in
    List.map walk c

(* substitute alpha with tau in a simple list *)
let subl alpha tau l =  
  List.map (singleSub alpha tau) l

(* unify the list of constraints c *)
let unify c =
  let rec checkDims dims = match dims with 
    | [] -> () 
    | (D_DimSize i, D_Dim a) :: lst 
    | (D_Dim a, D_DimSize i) :: lst when i <= a -> 
      checkDims lst
    | (D_DimSize i, D_Dim a) :: lst 
    | (D_Dim a, D_DimSize i) :: lst ->
      raise (DimSizeError (i,a))
    | (D_DimSize i, D_Alpha a) :: lst
    | (D_Alpha a, D_DimSize i) :: lst -> 
      raise (UnsolvedDimVar (D_Alpha a))
    | (D_DimSize _, D_DimSize _) :: _ 
    | (D_Alpha _, D_Alpha _) :: _ 
    | (D_Dim _, _) :: _ | (_, D_Dim _) :: _ ->
      internal "This must not happen"
  in
  let rec unifyDims dims dimsacc acc = match dims with
    | [] -> checkDims dimsacc; acc
    | (D_Dim a, D_Dim b) :: lst when a = b -> 
      unifyDims lst dimsacc acc 
    | (D_Dim a, D_Dim b) :: lst -> 
      raise (DimAccesError (a,b)) 
    | (D_DimSize i, D_Dim a) :: lst 
    | (D_Dim a, D_DimSize i) :: lst when i <= a -> 
      unifyDims lst dimsacc acc
    | (D_DimSize i, D_Dim a) :: lst 
    | (D_Dim a, D_DimSize i) :: lst ->
      raise (DimSizeError (i,a))
    | (D_DimSize s, D_Alpha a) :: lst 
    | (D_Alpha a, D_DimSize s) :: lst -> 
      unifyDims lst ((D_DimSize s, D_Alpha a) :: dimsacc) acc
    | (dim1, D_Alpha a) :: lst
    | (D_Alpha a, dim1) :: lst -> 
      unifyDims (subDim (D_Alpha a) dim1 lst)
        (subDim (D_Alpha a) dim1 dimsacc)
        (subArray (D_Alpha a) dim1 acc)  
    | (D_DimSize _, D_DimSize _) :: _ -> 
      internal "This must not happen"
  in         
  let rec unifyOrd ord = match ord with
    | [] -> ()
    | T_Int :: c | T_Float :: c | T_Char :: c -> 
      unifyOrd c 
    | (T_Alpha a) :: _ -> raise (UnsolvedTyVar (T_Alpha a))
    | typ :: _ -> 
      raise (TypeError ("Type does not support ordering", typ))
  in
  let rec unifyNofun nofun = match nofun with
    | [] -> ()
    | (T_Arrow (a, b)) :: c -> 
      raise (TypeError 
               ("Cannot return or compare function type", T_Arrow(a, b))
            )
    | (T_Alpha a) :: c -> raise (UnsolvedTyVar (T_Alpha a))
    | _ :: c -> 
      unifyNofun c
  in
  let rec unifyNoarr noarr = match noarr with
    | [] -> ()
    | (T_Array (a, b)) :: c -> 
      raise (TypeError ("Cannot compare array type", T_Array(a, b)))
    | (T_Alpha a) :: c -> raise (UnsolvedTyVar (T_Alpha a))
    | _ :: c -> 
      unifyNoarr c
  in
  let rec unifyAux c ord dims nofun noarr acc = match c with
    | [] -> 
      unifyOrd ord;
      unifyNofun nofun;
      unifyNoarr noarr;
      unifyDims (dims @ !dimConstraints) [] acc;
    | (tau1, tau2) :: c when tau1 = tau2 -> 
      unifyAux c ord dims nofun noarr acc
    | (T_Nofun, tau1) :: c | (tau1, T_Nofun) :: c ->
      unifyAux c ord dims (tau1 :: nofun) noarr acc 
    | (T_Noarr, tau1) :: c | (tau1, T_Noarr) :: c ->
      unifyAux c ord dims nofun (tau1 :: noarr) acc 
    | (T_Ord, tau1) :: c | (tau1, T_Ord) :: c -> 
      unifyAux c (tau1 :: ord) dims nofun noarr acc
    | (T_Ref tau1, T_Ref tau2) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord dims nofun noarr acc
    | (T_Array (tau1, dim1), T_Array (tau2, dim2)) :: c -> 
      unifyAux ((tau1, tau2) :: c) ord ((dim1, dim2) :: dims) nofun noarr acc
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
      unifyAux ((tau11, tau21) :: (tau12, tau22) :: c) ord dims nofun noarr acc
    | (T_Alpha alpha, tau1) :: c 
    | (tau1, T_Alpha alpha) :: c when notIn (T_Alpha alpha) tau1 -> 
      let sub_tlist = subc (T_Alpha alpha) tau1 in
      let sub_list = subl (T_Alpha alpha) tau1 in
        unifyAux (sub_tlist c) (sub_list ord) dims (sub_list nofun) 
          (sub_list noarr) ((T_Alpha alpha, tau1) :: (sub_tlist acc))
    | (typ1, typ2) :: lst -> raise (UnifyError (typ1,  typ2))
  in
  let solved = unifyAux c [] [] [] [] [] in
    if (debug_typeinf) then print_constraints solved;
    add_solved_table solved;
    solved 
