open Identifier
open Types

(** Symbol Table Types **)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE
  | PARDEF_DEFINE

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs  : int;
  mutable sco_hidden  : bool
}

and variable_info = {
  mutable variable_type    : typ;
  mutable variable_offset  : int;
  mutable variable_nesting : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_varlist   : entry list;
  mutable function_varsize   : int ref;          
  mutable function_paramsize : int;
  mutable function_result    : typ;
  mutable function_pstatus   : param_status;
  mutable function_nesting   : int;
  mutable function_parent    : entry option;
  mutable function_index     : int;
  mutable function_library   : bool
}

and parameter_info = {
  mutable parameter_type    : typ;
  mutable parameter_offset  : int;
  mutable parameter_nesting : int;
  parameter_mode            : pass_mode
}

and temporary_info = {
  temporary_type   : typ;
  temporary_offset : int
}

and constructor_info = {
  constructor_type      : typ;
  constructor_paramlist : typ list;
  constructor_tag       : int;
  constructor_arity     : int
}

and udt_info = {
  mutable udt_constructors : entry list;
  eq_function              : entry 
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info
               | ENTRY_udt of udt_info
               | ENTRY_constructor of constructor_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES


