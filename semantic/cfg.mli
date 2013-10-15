(** Creates the control flow graph on the quads intermediate form. *)

module LabelSet :
  sig
    type elt = Quads.Label.t
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
  end

module LS :
  sig
    include module type of LabelSet
    (** [print_set s] prints the elements (in this case labels) of 
       [s] *)
    val print_set : t -> string
  end

module Blocks :
  sig
  include module type of List
    type bblock = Quads.quad list
    type block_elt =
        SymbTypes.entry option * SymbTypes.entry option * LS.t * bblock
    (** An element of type [block_elt] is a quadraple of 
      {!SymbTypes.entry} [option] holding an optional entry if 
      a {!Quads.quad_operators} [Q_Unit] is present in the current block, 
      likewise for a [Q_Endu], a set {!Cfg.LabelSet.t} with the labels of the 
      quads in the block and finally the block of quads. *)
    type blocks = block_elt list
    val create_blocks : bblock -> blocks
    (** [create_blocks q] takes a list of {!Quads.quad} [[q1; ...;qn]]
       and creates the basic blocks as defined by type {!blocks}. *)
    val is_if : Quads.quad_operators -> bool
    (** Checks whether the operator is a relational operator *)
    val is_jmp : Quads.quad_operators -> bool
    (** Checks whether the operator is a jump operator *)
    val is_call : Quads.quad_operators -> bool
    (** Checks if the operator is a call operator *)
    val print_blocks : blocks -> unit
    (** Prints the basic blocks as calculated by {!create_blocks}. *)
    val dot_block : bblock -> string -> string
    (** Creates a basic block suitable for printing 
      to a .dot file for graphviz. Use {!Cfg.Dot} to get a .dot file  *)
  end

module V :
  sig
    type t = Blocks.bblock * LS.t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
  end

module G :
  sig
    type t = Graph.Persistent.Digraph.Concrete(V).t
    module V :
      sig
        type t = V.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = V.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = V.t * V.t
        val compare : t -> t -> int
        val src : t -> vertex
        val dst : t -> vertex
        type label = unit
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val empty : t
    val add_vertex : t -> vertex -> t
    val remove_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t
  end

module CFG :
  sig
    include module type of G
    type cfg = G.t
    type flow = G.edge
    type vblock = G.vertex
    val create_cfg : Quads.quad list -> cfg
    val print_vertices : G.t -> unit
    val print_edges : G.t -> unit
    val quads_of_cfg : G.t -> Quads.quad list
  end

module Dot :  
  sig    
    val fprint_graph :      
      Format.formatter -> Graph.Persistent.Digraph.Concrete(V).t -> unit    
    val output_graph :             
      out_channel -> Graph.Persistent.Digraph.Concrete(V).t -> unit          
  end   
