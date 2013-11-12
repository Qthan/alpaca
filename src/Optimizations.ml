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
  let (vertices, protected) = 
    CFG.fold_vertex (fun ((i, s, b) as v) (acc1, acc2) ->
        if Blocks.(i.entry_block) then 
          entry_ref := Some v;
        match Blocks.(i.protected) with
          | Some e -> (v :: acc1, v :: acc2)
          | None -> (v :: acc1, acc2) ) cfg ([], []) in
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
  let (reachable_cfg, reachable_vert) = 
    List.fold_left (fun (acc1, acc2) (i, s, b) as v -> 
        if not (List.exists (fun r -> V.equal r v) reachable) 
        then 
          (CFG.remove_vertex acc1 v, acc2)
        else
          (acc1, v :: acc2)) (cfg, []) vertices
  in
  (* adding function declarations required by protected blocks *)
  let (opt_cfg, _) = 
    List.fold_left (fun (cfg, new_reach) (i, s, b) ->
        match Blocks.(i.protected) with
          | None -> (cfg, new_reach)
          | Some f_unit ->
            (match CFG.safe_find_unit new_reach f_unit with
                Some v -> (cfg, new_reach)
              | None -> (* create a new block with dummy code *)
                let index = Blocks.counter in
                let block_info = 
                  Blocks.({ f_unit = Some f_unit;
                            f_endu = Some f_unit;
                            cur_fun = Some f_unit;
                            entry_block = false;
                            protected = None;
                            block_index = (incr index; !index)
                          })
                in
                let quad_lst = Quads.newQuadList () in
                let quad = 
                  Quads.(Q_Unit, O_Entry f_unit, O_Empty, O_Empty)
                in
                let quads1 = Quads.genQuad quad quad_lst in
                let quad = 
                  Quads.(Q_Endu, O_Entry f_unit, O_Empty, O_Empty)
                in
                let quads2 = Quads.genQuad quad quad_lst in
                let quads3 = Quads.mergeQuads quads1 quads2 in
                let v = CFG.V.create (block_info, LS.empty, quads3) 
                in
                  (CFG.add_vertex cfg v, v :: new_reach))) 
      (reachable_cfg, reachable_vert) reachable_vert
  in
    opt_cfg



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
