open Cfg
open Quads
open Blocks
open Error
open SymbTypes

type symVal = SymVal of int

type symExpr =
    Plus of symVal * symVal
  | Minus of symVal * symVal
  | Times of symVal * symVal
  | Div of symVal * symVal
  | Mod of symVal * symVal


module type EqualityType =
sig 
  type t
  val equal : t -> t -> bool
end

module type Dict =
sig
  type key
  type 'a t

  val empty   : unit -> 'a t
  val add     : 'a t -> key -> 'a -> 'a t
  val mem     : 'a t -> key -> bool
  val update  : 'a t -> key -> 'a -> 'a t
  val find  : 'a t -> key -> 'a option
end

module ListDict (Key : EqualityType) =
struct
  type key = Key.t
  type 'a t = (key * 'a) list

  let empty () = []
  let add d k v = (k, v) :: d
  let mem d k =
    let rec aux = function
      | [] -> false
      | (x, _) :: xs ->
        if Key.equal k x then true
        else aux xs
    in
      aux d
  let update d k v = d (* not needed for lists *)
  let find d k =
    let rec aux = function
      | [] -> None
      | (x, v) :: xs ->
        if Key.equal k x then Some v
        else aux xs
    in
      aux d
end


module VarMap = ListDict (struct type t = Quads.quad_operands
    let equal = Quads.operand_eq
  end)

module ExpMap = ListDict (struct type t = symExpr
    let equal = (=)
  end)

type maps = { 
  var_to_val : (symVal VarMap.t);
  exp_to_val : (symVal ExpMap.t);
  exp_to_tmp : (Quads.quad_operands ExpMap.t)
}

let new_SymVal = 
  let counter = ref 1 in
    (fun () -> 
       let a = !counter in
         incr counter;
         SymVal a)

let symExpr_of_bop sym1 sym2 = function
    Q_Plus | Q_Fplus -> Plus (sym1, sym2)
  | Q_Minus | Q_Fminus -> Minus (sym1, sym2)
  | Q_Mult | Q_Fmult -> Times (sym1, sym2)
  | Q_Div | Q_Fdiv -> Div (sym1, sym2)
  | Q_Mod -> Mod (sym1, sym2)
  | _ -> internal "Unsupported binary operator" (*see quads file*)

let simulate (info, s, block) =
  let maps = {
    var_to_val = VarMap.empty ();
    exp_to_val = ExpMap.empty ();
    exp_to_tmp = ExpMap.empty ()
  } 
  in 
  let rec aux block maps acc =
   (*Printf.printf "Block: %d\n" Blocks.(info.block_index);*)
    match block with
      | [] -> (info, s, Blocks.rev acc)
      | q :: qs when Quads.isBop q.operator ->
        let vtv = maps.var_to_val in
          (match VarMap.find vtv q.arg1, VarMap.find vtv q.arg2 with
            | None, None ->
              let sym1 = new_SymVal () in
              let sym2 = new_SymVal () in
              let var_to_val = VarMap.add vtv q.arg1 sym1 in 
              let var_to_val2 = VarMap.add var_to_val q.arg2 sym2 in
              let sym3 = new_SymVal () in
              let var_to_val3 = VarMap.add var_to_val2 q.arg3 sym3 in
              let symExpr = symExpr_of_bop sym1 sym2 q.operator in
              let exp_to_val = ExpMap.add maps.exp_to_val symExpr sym3 in
              let e = Quads.entry_of_quadop q.arg3 in
              let f = match info.cur_fun with
                  Some f -> f
                | None -> internal "I haven't stored the fun info, bad.."
              in
              let tmp = 
                Quads.newTemp (Intermediate.lookup_type (Some e)) f
              in
              let quad = Quads.genQuad (Q_Assign, q.arg3, O_Empty, tmp) [q] in
              let exp_to_tmp = ExpMap.add maps.exp_to_tmp symExpr tmp in
              let new_maps = 
                { var_to_val = var_to_val3;
                  exp_to_val = exp_to_val;
                  exp_to_tmp = exp_to_tmp
                }
              in
                aux qs new_maps (quad @ acc)
            | Some sym1, None ->
              let sym2 = new_SymVal () in
              let var_to_val = VarMap.add vtv q.arg2 sym2 in
              let sym3 = new_SymVal () in
              let var_to_val2 = VarMap.add var_to_val q.arg3 sym3 in
              let symExpr = symExpr_of_bop sym1 sym2 q.operator in
              let exp_to_val = ExpMap.add maps.exp_to_val symExpr sym3 in
              let e = Quads.entry_of_quadop q.arg3 in
              let f = match info.cur_fun with
                  Some f -> f
                | None -> internal "I haven't stored the fun info, bad.."
              in
              let tmp = 
                Quads.newTemp (Intermediate.lookup_type (Some e)) f
              in                
              let quad = Quads.genQuad (Q_Assign, q.arg3, O_Empty, tmp) [q] in
              let exp_to_tmp = ExpMap.add maps.exp_to_tmp symExpr tmp in
              let new_maps = 
                { var_to_val = var_to_val2;
                  exp_to_val = exp_to_val;
                  exp_to_tmp = exp_to_tmp
                }
              in
                aux qs new_maps (quad @ acc)
            | None, Some sym2 ->
              let sym1 = new_SymVal () in
              let var_to_val = VarMap.add vtv q.arg1 sym1 in
              let sym3 = new_SymVal () in
              let symExpr = symExpr_of_bop sym1 sym2 q.operator in
              let var_to_val2 = VarMap.add var_to_val q.arg3 sym3 in
              let exp_to_val = ExpMap.add maps.exp_to_val symExpr sym3 in
              let e = Quads.entry_of_quadop q.arg3 in
              let f = match info.cur_fun with
                  Some f -> f
                | None -> internal "I haven't stored the fun info, bad.."
              in
              let tmp = 
                Quads.newTemp (Intermediate.lookup_type (Some e)) f
              in                
              let quad = Quads.genQuad (Q_Assign, q.arg3, O_Empty, tmp) [q] in
              let exp_to_tmp = ExpMap.add maps.exp_to_tmp symExpr tmp in
              let new_maps = 
                { var_to_val = var_to_val2;
                  exp_to_val = exp_to_val;
                  exp_to_tmp = exp_to_tmp
                }
              in
                aux qs new_maps (quad @ acc)
            | Some sym1, Some sym2 ->
              let symExpr = symExpr_of_bop sym1 sym2 q.operator in
                (match ExpMap.find maps.exp_to_val symExpr with
                    None ->
                    let sym3 = new_SymVal () in
                    let exp_to_val = 
                      ExpMap.add maps.exp_to_val symExpr sym3 
                    in
                    let var_to_val =
                      VarMap.add maps.var_to_val q.arg3 sym3 in
                    let e = Quads.entry_of_quadop q.arg3 in
                    let f = match info.cur_fun with
                        Some f -> f
                      | None -> internal "I haven't stored the fun info, bad.."
                    in
                    let tmp = 
                      Quads.newTemp (Intermediate.lookup_type (Some e)) f
                    in                        
                    let quad = 
                      Quads.genQuad (Q_Assign, q.arg3, O_Empty, tmp) [q] 
                    in
                    let exp_to_tmp = 
                      ExpMap.add maps.exp_to_tmp symExpr tmp 
                    in
                    let new_maps = 
                      { var_to_val = var_to_val;
                        exp_to_val = exp_to_val;
                        exp_to_tmp = exp_to_tmp
                      }
                    in
                      aux qs new_maps (quad @ acc)
                  | Some sym3 ->
                    let var_to_val = VarMap.add vtv q.arg3 sym3 in
                    let tmp = match ExpMap.find maps.exp_to_tmp symExpr with
                        None -> internal "Temporary not found"
                      | Some tmp -> tmp
                    in 
                    let quad = 
                      Quads.genQuad (Q_Assign, tmp, O_Empty, q.arg3) []
                    in
                    let new_maps = 
                      { var_to_val = var_to_val;
                        exp_to_val = maps.exp_to_val;
                        exp_to_tmp = maps.exp_to_tmp
                      }
                    in
                      aux qs new_maps (quad @ acc)))
      | q :: qs -> aux qs maps (q :: acc)
  in
    aux block maps []
