(** Optimizations local to a block*)

type symVal = SymVal of int
type symExpr =
    Plus of symVal * symVal
  | Minus of symVal * symVal
  | Times of symVal * symVal
  | Div of symVal * symVal
  | Mod of symVal * symVal
module type EqualityType = sig type t val equal : t -> t -> bool end
module type Dict =
  sig
    type key
    type 'a t
    val empty : unit -> 'a t
    val add : 'a t -> key -> 'a -> 'a t
    val mem : 'a t -> key -> bool
    val update : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a option
    val remove : 'a t -> key -> 'a t
  end
module ListDict :
  functor (Key : EqualityType) ->
    sig
      type key = Key.t
      type 'a t = (key * 'a) list
      val empty : unit -> 'a list
      val add : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
      val mem : (Key.t * 'a) list -> Key.t -> bool
      val update : (Key.t * 'a) list -> Key.t -> 'a -> (Key.t * 'a) list
      val find : (Key.t * 'a) list -> Key.t -> 'a option
      val remove : (Key.t * 'a) list -> Key.t -> (Key.t * 'a) list
    end
module VarMap :
  sig
    type key = Quads.quad_operands
    type 'a t = (key * 'a) list
    val empty : unit -> 'a list
    val add : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
    val mem : (Quads.quad_operands * 'a) list -> Quads.quad_operands -> bool
    val update :
      (Quads.quad_operands * 'a) list ->
      Quads.quad_operands -> 'a -> (Quads.quad_operands * 'a) list
    val find :
      (Quads.quad_operands * 'a) list -> Quads.quad_operands -> 'a option
    val remove :
      (Quads.quad_operands * 'a) list ->
      Quads.quad_operands -> (Quads.quad_operands * 'a) list
  end
module ExpMap :
  sig
    type key = symExpr
    type 'a t = (key * 'a) list
    val empty : unit -> 'a list
    val add : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
    val mem : (symExpr * 'a) list -> symExpr -> bool
    val update : (symExpr * 'a) list -> symExpr -> 'a -> (symExpr * 'a) list
    val find : (symExpr * 'a) list -> symExpr -> 'a option
    val remove : (symExpr * 'a) list -> symExpr -> (symExpr * 'a) list
  end
type cse_maps = {
  var_to_val : symVal VarMap.t;
  exp_to_val : symVal ExpMap.t;
  exp_to_tmp : Quads.quad_operands ExpMap.t;
}
val new_SymVal : unit -> symVal
val symExpr_of_bop : symVal -> symVal -> Quads.quad_operators -> symExpr
val simulate :
  Cfg.Blocks.block_info * 'a * Quads.quad list ->
  Cfg.Blocks.block_info * 'a * Quads.quad list
type cp_maps = {
  tmp_to_var : Quads.quad_operands VarMap.t;
  var_to_tmp : Quads.quad_operands VarMap.t;
}
val propagate_var : Quads.quad -> cp_maps -> unit
val copy_propagate : 'a * 'b * Quads.quad list -> 'a * 'b * Quads.quad list
module TS :
  sig
    type elt = Quads.quad_operands
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
val deletable : Quads.quad_operands -> bool
val add_tmp : TS.elt -> TS.t -> TS.t
val dce :
  Cfg.Blocks.block_info * 'a * Quads.quad list ->
  Cfg.Blocks.block_info * 'a * Quads.quad list
