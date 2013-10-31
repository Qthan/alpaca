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
  type block_info = 
    { f_unit              : SymbTypes.entry option;
      f_endu              : SymbTypes.entry option;
      cur_fun             : SymbTypes.entry option;
      mutable block_index : int
    }

  (*  a record would be much better*)
  type block_elt = block_info * LS.t * bblock 
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

  let counter = ref 0

  (* Creates a list of triples block_elt.
   *  (only one per basic block), labels are the labels of the quads 
   *  that are included in the basic block and bblock the quads.
   *  The quads in each basic block are reversed,
   *  for (future) efficiency reasons.
   *  Assumes that relops are always followed by jumps so no attempt to "trap"
   *  relops is made.
   *  quads : List of quads
   *  f_unit : function entry that the current block belongs to
   *  cur_fun : function entry of Q_Unit if there is in current block
   *  cur_end : corresponding entry for Q_Endu 
   *  cur_set, cur_block : set of labels in quads block.
   *  Match expressions - not yet done. *)
  let rec create_blocks_aux quads ((cur_info, cur_set, cur_block) as cur) acc =
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
        let new_info = 
          { f_unit = None;
            f_endu = new_end;
            cur_fun = cur_info.cur_fun;
            block_index = 0
          }
        in
          if (cur_block = []) then
            create_blocks_aux qs (new_info, new_set, new_block) acc
          else 
            create_blocks_aux qs 
              (new_info, new_set, new_block) (cur :: acc)
      | q :: qs when (q.operator = Q_Unit) ->
        let new_fun = Some (Quads.entry_of_quadop q.arg1) in
        let new_info = 
          { f_unit = new_fun;
            f_endu = None;
            cur_fun = new_fun;
            block_index = 0
          }
        in
        let new_set = LS.singleton q.label in
        let new_block = q :: [] in
          if (cur_block = []) then
            create_blocks_aux qs (new_info, new_set, new_block) acc
          else
            create_blocks_aux qs 
              (new_info, new_set, new_block) (cur :: acc)
      | q :: qs when (q.operator = Q_Endu) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_end = Quads.entry_of_quadop q.arg1 in
        let new_info = 
          { f_unit = None;
            f_endu = None;
            cur_fun = cur_info.cur_fun;
            block_index = 0
          }
        in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
        let cur_info1 = { cur_info with f_endu = Some cur_end } in
          create_blocks_aux qs (new_info, new_set, new_block) 
            ((cur_info1, cur_set1, cur_block1) :: acc)
      | q :: qs when (new_flow q.operator = false) ->
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (cur_info, cur_set1, cur_block1) acc
      | q :: qs when (new_flow q.operator = true) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
        let new_info =
          { f_unit = None;
            f_endu = None;
            cur_fun = cur_info.cur_fun;
            block_index = 0
          }
        in
          create_blocks_aux qs (new_info, new_set, new_block) 
            ((cur_info, cur_set1, cur_block1) :: acc)
      | q :: qs -> 
        internal "You shouldn't bump here, \
                  but this function is not well-tested :)"

  let index_blocks blocks =
    iteri (fun i (info, _, _) -> info.block_index <- (i+1)) blocks

  let create_blocks quads =
    let cur_info =
      { 
        f_unit = None;
        f_endu = None;
        cur_fun = None;
        block_index = 0;
      }
    in
    let blocks =  create_blocks_aux quads (cur_info, LS.empty, []) [] in
    let () = index_blocks blocks in
      blocks

  let print_blocks blocks =
    iteri (fun i (f_info, s, q) ->
        match f_info.f_unit, f_info.f_endu with
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

  (* Returns a double escaped string from a string, useful for CFG visual*)
  let estring str = String.escaped (String.escaped str)

  let dot_block (info, _, quads) sep = 
    let rec aux quads acc =
      match quads with
        | [] -> acc
        | q :: qs ->
          let l = Quads.Label.label_of_string q.label in 
          let op = Quads.string_of_operator q.operator in
          let arg1 = estring (Quads.string_of_operand q.arg1) in
          let arg2 = estring (Quads.string_of_operand q.arg2) in
          let arg3 = estring (Quads.string_of_operand q.arg3) in
            aux qs (acc ^ l ^ ": " ^op ^ sep ^ arg1 
                    ^ sep ^ arg2 ^ sep ^ arg3 ^ "\\n")
    in
      aux quads (Printf.sprintf "Block: %d\n" info.block_index)


end

module V = 
struct
  type t = Blocks.block_elt
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


  let persistent_vmap f cfg =
    let tbl = Hashtbl.create 64 in
      fold_edges (fun v1 v2 acc ->
          let v1 = 
            if (Hashtbl.mem tbl v1) then Hashtbl.find tbl v1
            else
              begin
                let v1_new = f v1 in
                  Hashtbl.add tbl v1 v1_new;
                  v1_new
              end
          in
          let v2 =
            if (Hashtbl.mem tbl v2) then Hashtbl.find tbl v2
            else 
              begin
                let v2_new = f v2 in
                  Hashtbl.add tbl v2 v2_new;
                  v2_new
              end
          in
            add_edge acc v1 v2) cfg empty 



  let find_unit vertices e =
    let module B = Blocks in
    let vertex = 
      try
        B.find (fun (f_info, _, _) ->
            match B.(f_info.f_unit) with
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
    let module B = Blocks in
    let vertex = 
      try
        B.find (fun (f_info, _, _) ->
            match B.(f_info.f_endu) with
                None -> false
              | Some f -> Symbol.entry_eq e f) vertices
      with
          Not_found ->
          Format.fprintf (Format.std_formatter) 
            "entry not found : %a \n" print_entry e; 
          internal "entry not found"
    in
      vertex

  let find_next vertices next =
    let edge_to =
      try 
        List.find (fun (_, lset, _) ->
            LS.mem next lset) vertices
      with Not_found -> 
        internal "label %d not in any basic block!" next 
    in
      edge_to


  let add_flows qs vertices = 
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
          let edge_to = find_next vertices j_target in
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
                (* maybe library calls should be a dummy basic block 
                 * or maybe not.*)
                let next = q.label + 1 in
                let edge_to = find_next vertices next in
                  (edge_to :: out_f, in_f)
              | ENTRY_function f ->
                (* Edge from callee Q_Endu *)
                let edge_from = find_endu vertices c_target in
                (* Edge to callee Q_Unit*)
                let edge_to = find_unit vertices c_target in
                  (edge_to :: out_f, edge_from :: in_f)
              | ENTRY_variable _ | ENTRY_parameter _ ->
                (* high order functions for the now (and ever)
                 * generate an edge towards all functions! *)
                let (edges_to, edges_from) =
                  Blocks.fold_left 
                    (fun ((edge_to, edge_from) as acc) 
                      (callee_info, _, vertex) ->
                      match Blocks.(callee_info.f_unit, callee_info.f_endu) with
                        | None, None -> acc
                        | Some e, None ->
                          let v = find_unit vertices e in
                            (v :: edge_to, edge_from)
                        | None, Some e ->
                          let v = find_endu vertices e in
                            (edge_to, v :: edge_from)
                        | Some e1, Some e2 ->
                          let v1 = find_unit vertices e1 in
                          let v2 = find_endu vertices e2 in
                            (v1 :: edge_to, v2 :: edge_from)) ([],[]) vertices
                in
                  (edges_to @ out_f, edges_from @ in_f)
              | ENTRY_temporary _ | ENTRY_constructor _
              | ENTRY_none | ENTRY_udt _ ->
                internal "These entries cannot hold a function, hopefully"
            )
        | q :: qs when Quads.(q.operator = Q_Endu) -> (out_f, in_f)
        | q :: qs -> 
          let next = q.label + 1 in
          let edge_to = find_next vertices next in
            (edge_to :: out_f, in_f)
    in
      aux qs ([], [])

  let rec create_flows cfg vertices =
    fold_vertex (fun (i, s, v) cfg ->
        let (out_e, in_e) = add_flows v vertices in
        let cfg = 
          List.fold_left (fun cfg edge_to ->
              add_edge cfg (i, s, v) edge_to)
            cfg out_e 
        in
          (* add the return calls edges to the next quad *)
          match v with
            | q :: qs when Blocks.is_call q.operator -> 
              let next = q.label + 1 in
              let edge_return = find_next vertices next in
                List.fold_left (fun cfg edge_from ->
                    add_edge cfg edge_from edge_return)
                  cfg in_e
            | _ -> cfg) cfg cfg

  let create_vertices blocks = 
    Blocks.fold_left (fun (acc, cfg) (f_info, lset, b) -> 
        let vertex = V.create (f_info, lset, b) in
        let cfg = add_vertex cfg vertex in
        let acc = vertex :: acc in
          (acc, cfg)) 
      ([], empty) blocks

  let create_cfg quads =
    let blocks = Blocks.create_blocks quads in
    let (vertices, cfg) = create_vertices blocks in
    let cfg = create_flows cfg vertices in
      (* Normalize graph by reversing the quads in each basic block
       * (the future is here) *)
      persistent_vmap 
        (fun (f_info, lset, b) -> (f_info, lset, Blocks.rev b)) cfg

  let print_vertices cfg =
    iter_vertex (fun (_, _, v) -> Printf.printf "Block\n";
                  Quads.printQuads Format.std_formatter v) cfg

  let print_edges cfg =
    iter_edges (fun (_, _, v1) (_, _, v2) -> Printf.printf "Flows\n";
                 Quads.printQuads Format.std_formatter v1;
                 Printf.printf "\t|\n\t|\n\t|\n";
                 Quads.printQuads Format.std_formatter v2) cfg

  (* This implementation may require changes if we mess with the graph *)
  let quads_of_cfg cfg = 
    let v_list = fold_vertex (fun v acc -> v :: acc) cfg [] in
    let rec aux i acc =
      match (try Some List.find (fun (info, _, b) -> 
          Blocks.(info.block_index) = i) v_list 
         with Not_found -> None)
      with
          Some (_,_, b) -> aux (i+1) (acc @ b)
        | None -> acc
    in 
      aux 1 []


end

module Dot = Graph.Graphviz.Dot(struct
    include CFG (* use the graph module from above *)
    let edge_attributes e = [`Color 4711]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =
      let label = Blocks.dot_block v ", " in
        [`Shape `Box; `Label label; `Fontsize 11;]
    let vertex_name (i,_, _) = string_of_int Blocks.(i.block_index)
    let default_vertex_attributes _ = []
    let graph_attributes _ = [`Center true; `Nodesep 0.45; 
                              `Ranksep 0.45; `Size (42.67, 32.42)]
  end)

