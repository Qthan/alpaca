(** Performs optimizations on intermediate representation (quads) *)

module FS :
  sig
    type elt = SymbTypes.entry
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
  end
val do_opt : (Cfg.CFG.vblock -> Cfg.CFG.vblock) -> bool -> Cfg.G.t -> Cfg.G.t
val removable_temp : Quads.quad -> Quads.quad -> bool
val remove_temps :
  Cfg.Blocks.block_info * 'a * Quads.quad list ->
  Cfg.Blocks.block_info * 'a * Quads.quad list
val unreachable : Cfg.CFG.t -> Cfg.CFG.t
val optimize : Cfg.G.t -> Cfg.CFG.t
