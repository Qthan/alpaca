(** Infrastructure for creation of the Quads IR *)

module Label :
  sig
    type t = int
    val label : t ref
    val newLabel : unit -> t
    val nextLabel : unit -> t
    val label_of_string : t -> string
  end
module type LABEL_LIST =
  sig
    type labelList = Label.t list
    val newLabelList : unit -> labelList
    val makeLabelList : Label.t -> labelList
    val addLabel : Label.t -> labelList -> labelList
    val is_empty : labelList -> bool
    exception EmptyLabelList
    val removeLabel : labelList -> Label.t * labelList
    val peekLabel : labelList -> Label.t
    val mergeLabels : labelList -> labelList -> labelList
  end
module Labels : LABEL_LIST
type quad_operators =
    Q_Unit
  | Q_Endu
  | Q_Plus
  | Q_Minus
  | Q_Mult
  | Q_Div
  | Q_Mod
  | Q_Fplus
  | Q_Fminus
  | Q_Fmult
  | Q_Fdiv
  | Q_L
  | Q_Le
  | Q_G
  | Q_Ge
  | Q_Seq
  | Q_Nseq
  | Q_Eq
  | Q_Neq
  | Q_Assign
  | Q_Ifb
  | Q_Array
  | Q_Jump
  | Q_Jumpl
  | Q_Label
  | Q_Call
  | Q_Par
  | Q_Ret
  | Q_Dim
  | Q_Match
  | Q_Constr
  | Q_Fail
type quad_operands =
    O_Int of int
  | O_Float of float
  | O_Char of string
  | O_Bool of bool
  | O_Str of string
  | O_Backpatch
  | O_Label of int
  | O_Res
  | O_Ret
  | O_ByVal
  | O_Entry of SymbTypes.entry
  | O_Empty
  | O_Ref of quad_operands
  | O_Deref of quad_operands
  | O_Size of int
  | O_Dims of int
  | O_Index of quad_operands list
type quad = {
  label : Label.t;
  operator : quad_operators;
  arg1 : quad_operands;
  arg2 : quad_operands;
  mutable arg3 : quad_operands;
}
type expr_info = { place : quad_operands; next_expr : Labels.labelList; }
type cond_info = {
  true_lst : Labels.labelList;
  false_lst : Labels.labelList;
}
type stmt_info = { next_stmt : Labels.labelList; }
val labelsTbl : (int, int) Hashtbl.t
val memLabelTbl : int -> bool
val addLabelTbl : int -> unit
val newTemp : Types.typ -> SymbTypes.entry -> quad_operands
val getQuadBop : AstTypes.binop -> quad_operators
val getQuadUnop : AstTypes.unop -> quad_operators
val getQuadOpType : quad_operands -> Types.typ
val newQuadList : unit -> 'a list
val isEmptyQuadList : 'a list -> bool
val genQuad :
  quad_operators * quad_operands * quad_operands * quad_operands ->
  quad list -> quad list
val mergeQuads : 'a list -> 'a list -> 'a list
val setExprInfo : quad_operands -> Labels.labelList -> expr_info
val setCondInfo : Labels.labelList -> Labels.labelList -> cond_info
val setStmtInfo : Labels.labelList -> stmt_info
val backpatch : quad list -> Label.t list -> int -> quad list
val auxil_funs : (string * SymbTypes.entry) list
val findAuxilEntry : string -> SymbTypes.entry
val is_auxil_fun : Identifier.id -> bool
val string_of_operator : quad_operators -> string
val string_of_indexes : quad_operands list -> string
val string_of_entry : SymbTypes.entry -> string
val string_of_operand : quad_operands -> string
val print_operator : Format.formatter -> quad_operators -> unit
val print_entry : Format.formatter -> SymbTypes.entry -> unit
val print_indexes : Format.formatter -> quad_operands list -> unit
val print_operand : Format.formatter -> quad_operands -> unit
val entry_of_quadop : quad_operands -> SymbTypes.entry
val normalizeQuads : quad list -> quad list
val printQuad : Format.formatter -> quad -> unit
val printQuads : Format.formatter -> quad list -> unit
val isBop : quad_operators -> bool
val operand_eq : quad_operands -> quad_operands -> bool
