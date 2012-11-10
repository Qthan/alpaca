type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE                             (* ÐëÞñçò ïñéóìüò     *)
  | PARDEF_DEFINE                               (* Åí ìÝóù ïñéóìïý    *)
  | PARDEF_CHECK                                (* Åí ìÝóù åëÝã÷ïõ    *)

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool
}

and variable_info = {                         (******* ÌåôáâëçôÞ *******)
  mutable variable_type   : Types.typ;        (* Ôýðïò                 *)
  variable_offset         : int               (* Offset óôï Å.Ä.       *)
}

and function_info = {                         (******* ÓõíÜñôçóç *******)
  mutable function_isForward : bool;          (* ÄÞëùóç forward        *)
  mutable function_paramlist : entry list;    (* Ëßóôá ðáñáìÝôñùí      *)
  mutable function_redeflist : entry list;    (* Ëßóôá ðáñáìÝôñùí (2ç) *)
  mutable function_result    : Types.typ;     (* Ôýðïò áðïôåëÝóìáôïò   *)
  mutable function_pstatus   : param_status;  (* ÊáôÜóôáóç ðáñáìÝôñùí  *)
  mutable function_initquad  : int            (* Áñ÷éêÞ ôåôñÜäá        *)
}

and parameter_info = {                        (****** ÐáñÜìåôñïò *******)
  mutable parameter_type           : Types.typ;       (* Ôýðïò                 *)
  mutable parameter_offset : int;             (* Offset óôï Å.Ä.       *)
  parameter_mode           : pass_mode        (* Ôñüðïò ðåñÜóìáôïò     *)
}

and temporary_info = {                        (** ÐñïóùñéíÞ ìåôáâëçôÞ **)
  temporary_type   : Types.typ;               (* Ôýðïò                 *)
  temporary_offset : int                      (* Offset óôï Å.Ä.       *)
}
                       
and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

val currentScope : scope ref              (* ÔñÝ÷ïõóá åìâÝëåéá         *)
val quadNext : int ref                    (* Áñéèìüò åðüìåíçò ôåôñÜäáò *)
val tempNumber : int ref                  (* Áñßèìçóç ôùí temporaries  *)

val initSymbolTable  : int -> unit
val openScope        : unit -> unit
val closeScope       : unit -> unit
val hideScope        : scope -> bool -> unit
val newVariable      : Identifier.id -> Types.typ -> bool -> entry
val newFunction      : Identifier.id -> bool -> entry
val newParameter     : Identifier.id -> Types.typ -> pass_mode ->
                                        entry -> bool -> entry
val newTemporary     : Types.typ -> entry

val forwardFunction   : entry -> unit
val endFunctionHeader : entry -> Types.typ -> unit
val lookupEntry       : Identifier.id -> lookup_type -> bool -> entry

val start_positive_offset : int   (* Áñ÷éêü èåôéêü offset óôï Å.Ä.   *)
val start_negative_offset : int   (* Áñ÷éêü áñíçôéêü offset óôï Å.Ä. *)
val setType           : entry -> Types.typ  -> unit 
val getType           : entry -> Types.typ
                                                                  
