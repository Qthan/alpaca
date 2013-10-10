open Quads
open Error
open SymbTypes

(* A set of labels *)
module LabelSet = Set.Make (struct type t = Label.t 
    let compare = compare
  end)

module LS = struct
  include LabelSet
  let print_set s = 
    fold (fun e acc -> 
        (string_of_int e) ^ " " ^ acc) s ""
end

module Blocks =
struct
  include List
  type bblock = Quads.quad list
  type block_elt = SymbTypes.entry option * SymbTypes.entry option 
                   * LS.t * bblock
  type blocks = block_elt list

  (* Checks if quad op can change flow*)
  let rec new_flow op =
    (is_call op) || (is_jmp op)

  and is_call = function
      Q_Call -> true
    | _ -> false

  and is_jmp = function
      Q_Jump -> true
    | _ -> false

  and is_if = function
      Q_L | Q_Le | Q_G
    | Q_Ge | Q_Seq | Q_Nseq
    | Q_Eq | Q_Neq -> true
    | _ -> false

  (* Creates a list of triples (function entry, labels, bblock).
   *  Function entry is Some entry if a function is defined in this block
   *  (only one per basic block), labels are the labels of the quads 
   *  that are included in the basic block and bblock the quads.
   *  The quads in each basic block are reversed,
   *  for (future) efficiency reasons.
   *  Assumes that relops are always followed by jumps so no attempt to "trap"
   *  relops is made.
   *  quads : List of quads
   *  cur_fun : function entry of Q_Unit if applicable to current block
   *  cur_end : corresponding entry for Q_Endu 
   *  cur_set, cur_block : set of labels in quads block.
   *  Match expressions - not yet done. *)
  let rec create_blocks_aux quads 
      ((cur_fun, cur_end, cur_set, cur_block) as cur) acc =
    match quads with
      | [] -> 
        if (cur_block = []) then rev acc
        else rev (cur :: acc)
      | q :: qs when (Quads.memLabelTbl q.label = true) ->
        let new_set = LS.singleton q.label in
        let new_block = q :: [] in
        let new_end = match q.operator with
          | Q_Endu -> Some (Quads.entry_of_quadop q.arg1)
          | _ -> None
        in
          if (cur_block = []) then
            create_blocks_aux qs (None, new_end, new_set, new_block) acc
          else 
            create_blocks_aux qs 
              (None, new_end, new_set, new_block) (cur :: acc)
      | q :: qs when (q.operator = Q_Unit) ->
        let new_fun = Quads.entry_of_quadop q.arg1 in
        let new_set = LS.singleton q.label in
        let new_block = q :: [] in
          if (cur_block = []) then
            create_blocks_aux qs (Some new_fun, None, new_set, new_block) acc
          else
            create_blocks_aux qs 
              (Some new_fun, None, new_set, new_block) (cur :: acc)
      | q :: qs when (q.operator = Q_Endu) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_end = Quads.entry_of_quadop q.arg1 in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (None, None, new_set, new_block) 
            ((cur_fun, Some cur_end, cur_set1, cur_block1) :: acc)
      | q :: qs when (new_flow q.operator = false) ->
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (cur_fun, cur_end, cur_set1, cur_block1) acc
      | q :: qs when (new_flow q.operator = true) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (None, None, new_set, new_block) 
            ((cur_fun, cur_end, cur_set1, cur_block1) :: acc)

  let create_blocks quads =
    create_blocks_aux quads (None, None, LS.empty, []) []

  let print_blocks blocks =
    iteri (fun i (f_u, f_endu, s, q) ->
        match f_u, f_endu with
            None, None -> 
            Format.fprintf (Format.std_formatter) 
              "Basic Block %d\n \
               Labels : %s\n %a" i (LS.print_set s) printQuads q
          | Some f_unit, None ->
            Format.fprintf (Format.std_formatter) 
              "Basic Block %d\n Fun : %a\n
                    Labels : %s\n %a" i Quads.print_entry f_unit
              (LS.print_set s) printQuads q
          | None, Some f_endu  ->
            Format.fprintf (Format.std_formatter) 
              "Basic Block %d\n EndFun : %a\n
                    Labels : %s\n %a" i Quads.print_entry f_endu
              (LS.print_set s) printQuads q
          | Some f_unit, Some f_endu ->
            Format.fprintf (Format.std_formatter) 
              "Basic Block %d\n Fun : %a\n EndFun : %a\n \
               Labels : %s\n %a" i Quads.print_entry f_unit 
              Quads.print_entry f_endu
              (LS.print_set s) printQuads q) blocks
end

module V = 
struct
  type t = Blocks.bblock
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Persistent.Digraph.Concrete (V)

module CFG =
struct
  include G
  type cfg = G.t
  type flow = G.edge
  type vblock = G.vertex

  let find_unit vertices e =
    let (_, _, _, vertex) = 
      try
        Blocks.find (fun (f_unit, _, _, vertex) ->
            match f_unit with
                None -> false
              | Some f -> Symbol.entry_eq e f) vertices
      with
          Not_found ->
          Format.fprintf (Format.std_formatter) 
            "entry not found : %a \n" print_entry e; 
          internal "entry not found"
    in
      vertex

  let find_endu vertices e =
    let (_, _, _, vertex) = 
      try
        Blocks.find (fun (_, f_endu, _, vertex) ->
            match f_endu with
                None -> false
              | Some f -> Symbol.entry_eq e f) vertices
      with
          Not_found ->
          Format.fprintf (Format.std_formatter) 
            "entry not found : %a \n" print_entry e; 
          internal "entry not found"
    in
      vertex

  let add_flows v vertices = 
    (* in_flows comes from call and represents a return to call site *)
    let rec aux qs (out_f, in_f) =
      match qs with
        | [] -> (out_f, in_f)
        | q :: qs when (Blocks.is_if q.operator) || 
                       (Blocks.is_jmp q.operator) ->
          let j_target = match q.arg3 with
            | O_Label i -> i
            | _ -> internal "Must jump to label"
          in
          let (_, _, _, edge_to) =
            try
              List.find (fun (_, _, lset, vertex) ->
                  LS.mem j_target lset) vertices
            with Not_found -> internal "label not in any basic block!"
          in
            (* Recursively call aux if more flow changing commands follow*)
            (match qs with
              | [] -> (edge_to :: out_f, in_f)
              | q2 :: qss when Blocks.is_if q2.operator -> 
                aux qs (edge_to :: out_f, in_f)
              | q2 :: qss -> (edge_to :: out_f, in_f))
        | q :: qs when (Blocks.is_call q.operator) ->
          let c_target = Quads.entry_of_quadop q.arg3 in
            (match c_target.entry_info with
              | ENTRY_function _ when Symbol.isLibraryFunction c_target 
                                   || Quads.is_auxil_fun c_target.entry_id ->
                (out_f, in_f) (*no new edges created by
                               * calls to library functions *)
              | ENTRY_function f ->
                (* Edge from callee Q_Endu *)
                let edge_from = find_endu vertices c_target in
                (* Edge to callee Q_Unit*)
                let edge_to = find_unit vertices c_target in
                  aux qs (edge_to :: out_f, edge_from :: in_f)
              | ENTRY_variable _ | ENTRY_parameter _ ->
                (* high order functions for the now (and ever)
                 * generate an edge towards all functions! *)
                let (edges_to, edges_from) =
                  Blocks.fold_left 
                    (fun ((edge_to, edge_from) as acc) 
                      (callee_u, callee_endu, _, vertex) ->
                      match callee_u, callee_endu with
                        | None, None -> acc
                        | Some e, None ->
                          let v = find_unit vertices e in
                            (v :: edge_to, edge_from)
                        | _, Some e ->
                          let v = find_endu vertices e in
                            (edge_to, v :: edge_from)
                        | Some e1, Some e2 ->
                          let v1 = find_unit vertices e1 in
                          let v2 = find_endu vertices e2 in
                            (v1 :: edge_to, v2 :: edge_from)) ([],[]) vertices
                in
                  (edges_to @ out_f, edges_from @ in_f))
        | q :: qs when qs <> [] -> 
          let next = q.label + 1 in
          let (_, _, _, edge_to) =
            try 
              List.find (fun (_, _, lset, vertex) ->
                  LS.mem next lset) vertices
            with Not_found -> internal "label not in any basic block!"
          in
            (edge_to :: out_f, in_f)
        | q :: qs -> (out_f, in_f)
    in
      aux v ([], [])

  let rec create_flows cfg vertices =
    fold_vertex (fun v cfg ->
        let (out_e, in_e) = add_flows v vertices in
        let cfg = 
          List.fold_left (fun cfg edge_to ->
              add_edge cfg v edge_to)
            cfg out_e 
        in
          List.fold_left (fun cfg edge_from ->
              add_edge cfg edge_from v)
            cfg in_e) cfg cfg

  let create_vertices blocks : Blocks.blocks * cfg = 
    Blocks.fold_left (fun (acc, cfg) (f_unit, f_endu, lset, b) -> 
        let vertex = V.create b in
        let cfg = add_vertex cfg vertex in
        let acc = (f_unit, f_endu, lset, vertex) :: acc in
          (acc, cfg)) 
      ([], empty) blocks

  let create_cfg quads =
    let blocks = Blocks.create_blocks quads in
    let (vertices, cfg) = create_vertices blocks in
    let cfg = create_flows cfg vertices in
      (* Normalize graph by reversing the quads in each basic block
       * (the future is here) *)
      map_vertex (fun v -> v) cfg

  let print_vertices cfg =
    iter_vertex (fun v -> Printf.printf "Block\n";
                  Quads.printQuads Format.std_formatter v) cfg

end

(*not yet ready *)
module Dot = Graph.Graphviz.Dot(struct
   include CFG (* use the graph module from above *)
   let edge_attributes e = [`Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v = "st"
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

