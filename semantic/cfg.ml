open Quads
open Error

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

module Blocks :
sig
  include module type of List
  type bblock = Quads.quad list
  type blocks = (SymbTypes.entry option * LS.t * bblock) list
  val create_blocks_aux : bblock -> SymbTypes.entry option * LS.t * bblock 
    -> blocks -> blocks
  val create_blocks : Quads.quad list -> blocks
  val is_if : Quads.quad_operators -> bool
  val is_jmp : Quads.quad_operators -> bool
  val is_call : Quads.quad_operators -> bool
  val to_list : blocks -> (SymbTypes.entry option * LS.t * bblock) list
  val print_blocks : blocks -> unit
end =
struct
  include List
  type bblock = Quads.quad list
  type blocks = (SymbTypes.entry option * LS.t * bblock) list

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
   *  Match expressions - not yet done. *)
  let rec create_blocks_aux quads ((cur_fun, cur_set, cur_block) as cur) acc =
    match quads with
      | [] -> rev acc
      | q :: qs when (Quads.memLabelTbl q.label = true) ->
        let new_set = LS.singleton q.label in
        let new_block = q :: [] in
          create_blocks_aux qs (None, new_set, new_block) 
            (cur :: acc)
      | q :: qs when (q.operator = Q_Unit) ->
        let new_fun = Quads.entry_of_quadop q.arg1 in
        let new_set = LS.singleton q.label in
        let new_block = q :: [] in
          if (cur_block = []) then
            create_blocks_aux qs (Some new_fun, new_set, new_block) acc
          else
            create_blocks_aux qs (Some new_fun, new_set, new_block) (cur :: acc)
      | q :: qs when (q.operator = Q_Endu) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (None, new_set, new_block) 
            ((cur_fun, cur_set1, cur_block1) :: acc)
      | q :: qs when (new_flow q.operator = false) ->
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (cur_fun, cur_set1, cur_block1) acc
      | q :: qs when (new_flow q.operator = true) ->
        let new_set = LS.empty in
        let new_block = [] in
        let cur_set1 = LS.add q.label cur_set in
        let cur_block1 = q :: cur_block in
          create_blocks_aux qs (None, new_set, new_block) 
            ((cur_fun, cur_set1, cur_block1) :: acc)

  let create_blocks quads =
    create_blocks_aux quads (None, LS.empty, []) []

  let print_blocks blocks =
    iteri (fun i (_, s, q) -> 
        Format.fprintf (Format.std_formatter) 
          "Basic Block %d\n \
          Labels : %s\n %a" i (LS.print_set s) printQuads q) blocks

  let to_list x = x

end

module V : 
sig
  type t = Blocks.bblock
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end = 
struct
  type t = Blocks.bblock
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Persistent.Digraph.Concrete (V)

module CFG :
sig
  include module type of G
  type cfg = G.t
  type flow = G.edge
  type vblock = G.vertex
  val create_cfg : Quads.quad list -> cfg
  val print_vertices : G.t -> unit
end =
struct
  type cfg = G.t
  type flow = G.edge
  type vblock = G.vertex
  include G

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
          let (_, _, edge_to) = 
            List.find (fun (_, lset, vertex) ->
                LS.mem j_target lset) vertices
          in
            aux qs (edge_to :: out_f, in_f)
        | q :: qs when (Blocks.is_call q.operator) ->
          let c_target = Quads.entry_of_quadop q.arg3 in
          let (_, _, edge_bi) = 
            List.find (fun (e, _, vertex) -> (Some c_target) = e) vertices 
            (*unsure about type of equality*)
          in
            aux qs (edge_bi :: out_f, edge_bi :: in_f)
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

  let create_vertices blocks : 
    (SymbTypes.entry option * LS.t * vblock) list * cfg = 
    Blocks.fold_left (fun (acc, cfg) (e, lset, b) -> 
        let vertex = V.create b in
        let cfg = add_vertex cfg vertex in
        let acc = (e, lset, vertex) :: acc in
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

