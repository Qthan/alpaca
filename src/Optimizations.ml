open Cfg
open SymbTypes
open Error
open Quads
open Misc

module FS = Set.Make (struct type t = SymbTypes.entry
    let compare = compare
  end)

let do_opt f fix cfg =
  let fixTmpOffsets e = match e.entry_info with
    | ENTRY_function f -> Symbol.fixTmpOffsets f
    | _ -> internal "only functions applicable"
  in
  let optimized_cfg = CFG.persistent_vmap f cfg in
    if fix then
      begin
        let funs = CFG.fold_vertex (fun (info, _, _) acc ->
            match Blocks.(info.cur_fun) with
                Some f ->
                FS.add f acc
              | None -> internal "shouldn't happen") 
            cfg FS.empty 
        in
          FS.iter fixTmpOffsets funs;
      end;
    optimized_cfg

let removable_temp q q1 =
  Quads.isBop q.operator && q.arg3 = q1.arg1 && q1.operator = Q_Assign

let remove_temps (info, s, b) =
  let rec aux b acc =
    match b with
        [] -> (info, s, Blocks.rev acc)
      | q :: q1 :: qs when removable_temp q q1 ->
        let tmp_e = Quads.deep_entry_of_quadop q.arg3 in
        let e_f = match Blocks.(info.cur_fun) with
          | None -> internal "there should be a function entry here..."
          | Some e_f -> e_f
        in          
        let () = Symbol.removeTemp tmp_e e_f in
        let () = q.arg3 <- q1.arg3 in
          aux qs (q :: acc)
      | q :: qs -> aux qs (q :: acc)
  in
    aux b []

(* DFS *)
let unreachable cfg =
  let entry_ref = ref None in
  let vertices = CFG.fold_vertex (fun ((i, s, b) as v) acc ->
                                    if Blocks.(i.entry_block) then 
                                      entry_ref := Some v;
                                    v :: acc) cfg [] in
  let entry_vert = match !entry_ref with
      None -> internal "No entry block"
    | Some v -> v
  in
  let rec search stack visited =
    match stack with
      | [] -> visited
      | v :: vs -> 
          let children = CFG.succ cfg v in
          let unexplored = 
            List.filter (fun c 
                         -> not (List.exists (fun v -> V.equal v c) visited)) 
              children
          in
            search (unexplored @ vs) (v :: visited)
  in
  let reachable = search [entry_vert] [] in
    List.fold_left (fun acc (i, s, b) as v -> 
                      if not (List.exists (fun r -> V.equal r v) reachable) 
                      then 
                        CFG.remove_vertex acc v
                      else
                        acc) cfg vertices 


let optimize cfg =
  let opt_cfg = 
    cfg
    |> do_opt remove_temps true 
    |> do_opt Local.simulate false
    |> do_opt Local.copy_propagate false
    |> do_opt Local.dce true
  in
  let opt_ir = Quads.normalizeQuads (CFG.quads_of_cfg opt_cfg) in
  let opt_cfg = CFG.create_cfg opt_ir in
    unreachable opt_cfg
