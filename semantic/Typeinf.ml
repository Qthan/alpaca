open Types 

let fresh =
  let k = ref 0 in
    fun () -> incr k; T_Alpha !k

let refresh ty = 
  match ty with
    | T_Notype -> fresh ()
    | _        -> ty

let rec unify c = match c with
    | [] -> []
    | (tau1, tau2) :: c when tau1 = tau2 -> unify c
    | (T_Alpha alpha, tau2) :: c when notfree alpha tau2 ->
      (alpha, tau2) :: unify (subc alpha tau2 c)
    | (tau1, T_Alpha alpha) :: c when notfree alpha tau1 ->
      (alpha, tau1) :: unify (subc alpha tau1 c)
    | (T_Arrow (tau11, tau12), T_Arrow (tau21, tau22)) :: c ->
      unify ((tau11, tau21) :: (tau12, tau22) :: c)
    | _ -> failwith "ERROR !!! BOO !!!"


let rec notIn alpha typ = match typ with
  | T_Alpha n -> alpha != (T_Alpha n)
  | T_Arrow (t1, t2) -> (notIn alpha t1) && (notIn alpha t2)
  | T_Array(a,n) -> a != alpha 
  | T_Ref tref -> tref != alpha
  | T_Int  | T_Char  | T_Str
  | T_Bool | T_Float | T_Notype -> true
  
  
let rec subc alpha tau c =
  let walk (tau1, tau2) = (singleSub alpha tau tau1, singleSub alpha tau tau2) in
    List.map walk c
    
let rec singleSub alpha t2 typ = match alpha, typ with
      | T_Alpha a, T_Alpha n when a = n -> t2
      | T_Alpha _, T_Alpha _ -> typ
      | T_Alpha _, T_Arrow (typ1,typ2) -> T_Arrow ((singleSub alpha t2 typ1),(singleSub alpha t2 typ2))
      | T_Alpha _, T_Ref typ1-> T_Ref (singleSub alpha t2 typ1)
      | T_Alpha _, T_Array (typ1,n)-> T_Array ((singleSub alpha t2 typ1),n)
      | T_Alpha _, _ -> typ
      | _, _ -> internal "must be alpha"
