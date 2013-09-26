open Format
open Types
open AstTypes
open Identifier
open Error
open SymbTypes
open Symbol
open Pretty_print

(* Label lists interface *)
(*module type LABEL_LIST =  
  sig
  type labelList
  (* create an empty labelList *)
  val newLabelList : unit -> labelList
  (* create a labelList with label n *) 
  val makeLabelList : int -> labelList
  (* add a new label to labelList *)
  val addLabel : int -> labelList -> labelList
  (* return true if no labels are stored *)
  val is_empty : labelList -> bool
  (* raised on remove/peek of empty labelList *) 
  exception EmptyLabelList
  (* retrieve first element and return rest of labelList *)
  val removeLabel : labelList -> int * labelList
  (* peek at first element *)
  val peekLabel : labelList -> int
  val mergeLabels : labelList -> labelList -> labelList
  end
*)

type labelList = int list
exception EmptyLabelList
let newLabelList () : labelList = []
let makeLabelList (n : int) = [n]
let addLabel (n : int) (l : labelList) = n :: l
let is_empty (l : labelList) = l = []
let removeLabel (l : labelList) =
  match l with 
    | [] -> raise EmptyLabelList
    | n :: t -> (n, t)
let peekLabel (l : labelList) = 
  match l with
    | [] -> raise EmptyLabelList
    | n :: _ -> n
let mergeLabels (l1 : labelList) (l2 : labelList) =
  l1 @ l2



(* Intermediate Types *)
type temp_header = {
  temp_name : int;
  temp_type : typ;
  temp_offset : int
}

type quad_operators =
  | Q_Unit | Q_Endu
  | Q_Plus | Q_Minus | Q_Mult | Q_Div | Q_Mod
  | Q_Fplus | Q_Fminus | Q_Fmult | Q_Fdiv 
  | Q_L | Q_Le | Q_G | Q_Ge | Q_Seq | Q_Nseq
  | Q_Eq | Q_Neq (* Physical equality *)
  | Q_Assign | Q_Ifb | Q_Array
  | Q_Jump | Q_Jumpl | Q_Label
  | Q_Call | Q_Par | Q_Ret | Q_Dim
  | Q_Match | Q_Constr | Q_Fail

type quad_operands = 
  | O_Int of int
  | O_Float of float
  | O_Char of string
  | O_Bool of bool
  | O_Str of string 
  | O_Backpatch
  | O_Label of int
  | O_Res (* $$ *)
  | O_Ret (* RET *)
  | O_ByVal
  | O_Entry of entry 
  | O_Empty
  | O_Ref of quad_operands
  | O_Deref of quad_operands
  | O_Size of int
  | O_Dims of int
  | O_Index of quad_operands list

type quad = {
  label : int;
  operator : quad_operators;
  arg1 : quad_operands;
  arg2 : quad_operands;
  mutable arg3 : quad_operands
}

type expr_info = {
  place : quad_operands;
  next_expr  : labelList
}

type cond_info = {
  true_lst  : labelList;
  false_lst : labelList
}

type stmt_info = { 
  next_stmt : labelList
}

(* Quads infrastructure *)

let label = ref 0 

let newLabel = fun () -> incr label; !label

let nextLabel = fun () -> !label + 1

let labelsTbl = Hashtbl.create 101 

(* Modularity *)
let memLabelTbl label = Hashtbl.mem labelsTbl label 
let addLabelTbl label = Hashtbl.replace labelsTbl label 0

let newTemp =
  let k = ref 1 in
    fun typ size -> 
      let tempsize = sizeOfType typ in 
        size := !size + tempsize; 
        let header = {  
          entry_id = id_make ("$" ^ string_of_int !k);
          entry_scope = 
            {
              sco_parent = None;
              sco_nesting = 0;
              sco_entries = [];
              sco_negofs  = 0;
              sco_hidden = false;
            };
          entry_info = 
            ENTRY_temporary {
              temporary_type = typ;
              temporary_offset = - !size
            }
        }
        in
          incr k;
          O_Entry header

(* Return quad operator from Llama binary operator *)
let getQuadBop bop = match bop with 
  | Plus -> Q_Plus 
  | Fplus -> Q_Fplus
  | Minus -> Q_Minus
  | Fminus -> Q_Fminus
  | Times -> Q_Mult
  | Ftimes -> Q_Fmult
  | Div  -> Q_Div
  | Fdiv -> Q_Fdiv
  | Mod  -> Q_Mod
  | Seq -> Q_Seq 
  | Nseq -> Q_Nseq
  | L -> Q_L
  | Le -> Q_Le
  | G -> Q_G
  | Ge -> Q_Ge
  | Eq -> Q_Eq
  | Neq -> Q_Neq
  | And | Or | Semicolon | Power -> internal "no operator for and/or/;/pow" 
  | Assign -> Q_Assign

let getQuadUnop unop = match unop with
  | U_Plus -> Q_Plus
  | U_Minus -> Q_Minus
  | U_Fplus -> Q_Fplus
  | U_Fminus -> Q_Fminus
  | U_Not | U_Del -> internal "no operator for not/delete"

let rec getQuadOpType operand =
  match operand with
    | O_Int _ -> T_Int
    | O_Float _ -> T_Float
    | O_Char _ -> T_Char
    | O_Bool _ -> T_Bool
    | O_Str _ -> T_Array (T_Char, D_Int 1)
    | O_Backpatch -> internal "Backpatch? Here?"
    | O_Label _ -> internal "But a label? Here?"
    | O_Res -> internal "Res? Here?" (* $$ *)
    | O_Ret -> internal "Ret? Here?" (* RET *)
    | O_ByVal -> internal "By Val? Here?"
    | O_Entry e -> getType e
    | O_Empty -> internal "Empty? Here"
    | O_Ref op -> T_Ref (getQuadOpType op)
    | O_Deref op -> 
      (match getQuadOpType op with
        | T_Ref typ -> typ 
        | _ -> internal "Cannot dereference a no reference")
    | O_Size _ -> internal "Size? Here?"
    | O_Dims _ -> internal "Dims? Dims here?"
    | O_Index _ -> internal "Shouldn't be here, something went wrong"

let newQuadList () = []
let isEmptyQuadList quads = quads = []

let genQuad (op, ar1, ar2, ar3) quad_lst =
  let quad = {
    label = newLabel ();
    operator = op;
    arg1 = ar1;
    arg2 = ar2;
    arg3 = ar3
  } 
  in
    (quad :: quad_lst) 

let mergeQuads quads new_quads = quads @ new_quads

let setExprInfo p n = { place = p; next_expr = n }

let setCondInfo t f = { true_lst = t; false_lst = f }

let setStmtInfo n = { next_stmt = n }

(* XXX Backpatch, changes a mutable field so we can maybe avoid returning a new
 * quad list thus avoiding all the quads1,2,3... pollution. Moo XXX*)
let backpatch quads lst patch =
  if (not (isEmptyQuadList lst)) then addLabelTbl patch; 
  List.iter (fun quad_label -> 
      match (try Some (List.find (fun q -> q.label = quad_label) quads) 
             with Not_found -> None) with
        | None -> internal "Quad label not found, can't backpatch\n"
        | Some quad -> quad.arg3 <- O_Label patch) lst;
  quads

let entry_of_quadop op = match op with 
  | O_Entry e -> e
  | _ -> internal "expecting entry"

let auxil_funs =
  let makeEntry id size typ = { 
    entry_id = (id_make id);
    entry_scope = 
      {
        sco_parent = None;
        sco_nesting = 0;
        sco_entries = [];
        sco_negofs  = 0;
        sco_hidden = false;
      };
    entry_info = 
      ENTRY_function {
        function_isForward = false;
        function_paramlist = [];
        function_varlist = [];
        function_varsize = ref 0;
        function_paramsize = size;       
        function_result = typ;
        function_pstatus = PARDEF_COMPLETE;
        function_nesting = 0;
        function_parent = None;
        function_index = -1;
        function_library = true
      } } 
  in
    [ ("_make_array", makeEntry "_make_array" 4 (T_Array (T_Unit, D_Int 1)));
      ("_delete_array", makeEntry "_delete_array" 2 T_Unit);
      ("_new", makeEntry "_new" 2 (T_Ref T_Int));
      ("_delete", makeEntry "_delete" 2 T_Unit);
      ("_dummy", makeEntry "_dummy" 0 T_Unit);
      ("_pow", makeEntry "_pow" 20 T_Float)]

let findAuxilEntry id = List.assoc id auxil_funs

let string_of_operator = function 
  | Q_Unit -> "Unit" 
  | Q_Endu -> "Endu"
  | Q_Plus -> "+" 
  | Q_Minus -> "-" 
  | Q_Mult -> "*" 
  | Q_Div -> "/" 
  | Q_Mod -> "Mod"
  | Q_Fplus -> "+."
  | Q_Fminus -> "-." 
  | Q_Fmult -> "*."
  | Q_Fdiv -> "/." 
  | Q_L -> "<"
  | Q_Le -> "<=" 
  | Q_G -> ">" 
  | Q_Ge -> ">=" 
  | Q_Seq -> "=" 
  | Q_Nseq -> "<>"
  | Q_Eq -> "==" 
  | Q_Neq -> "!=" (* Physical equality *)
  | Q_Dim -> "dim"
  | Q_Assign -> ":=" | Q_Ifb -> "ifb" | Q_Array -> "Array"
  | Q_Jump -> "Jump" | Q_Jumpl -> "Jumpl" | Q_Label -> "Label??"
  | Q_Call -> "call" | Q_Par -> "par" | Q_Ret -> "Ret??" 
  | Q_Match -> "match" | Q_Constr -> "constr" | Q_Fail -> "fail"
  
let print_operator chan op = fprintf chan "%s" (string_of_operator op)

let print_entry chan entry =
  match entry.entry_info with
    | ENTRY_function f ->
      let parent_id = match f.function_parent with
        | Some e -> e.entry_id
        | None -> id_make "None"
      in
        fprintf chan "Fun[%a, index %d, params %d, vars %d, nest %d, parent %a]" 
                pretty_id entry.entry_id
                f.function_index f.function_paramsize 
                !(f.function_varsize) f.function_nesting
                pretty_id parent_id
    | ENTRY_variable v -> 
      fprintf chan "Var[%a, type %a, offset %d, nest %d]" 
              pretty_id entry.entry_id pretty_typ v.variable_type 
              v.variable_offset v.variable_nesting
    | ENTRY_parameter p -> 
      fprintf chan "Par[%a, type %a, offset %d, nest %d]" 
              pretty_id entry.entry_id pretty_typ p.parameter_type 
              p.parameter_offset p.parameter_nesting
    | ENTRY_temporary t ->
      fprintf chan "Temp[%a, type %a, offset %d]" 
              pretty_id entry.entry_id pretty_typ t.temporary_type 
              t.temporary_offset
    | ENTRY_constructor c ->
      fprintf chan "Constr[%a, type %a, arity %d, tag %d]" 
              pretty_id entry.entry_id pretty_typ c.constructor_type
              c.constructor_arity c.constructor_tag
    | ENTRY_udt u -> internal "UDT entries should not be visible to user"
    | ENTRY_none -> internal "Error, tried to access empty entry"


let print_temp_head chan head = 
  fprintf chan "[%d, %a, %d]" head.temp_name pretty_typ 
    head.temp_type head.temp_offset 

let rec print_indexes chan lst =
  let rec pp_indexes ppf lst =
    match lst with
      | [] -> ()
      | x :: [] -> fprintf ppf "%a" print_operand x
      | x :: xs -> fprintf ppf "%a, %a" print_operand x pp_indexes xs
  in
    fprintf chan "%a" pp_indexes lst
    
and print_operand chan op = match op with
  | O_Int i -> fprintf chan "%d" i 
  | O_Float f -> fprintf chan "%f" f 
  | O_Char str -> fprintf chan "\'%s\'" str 
  | O_Bool b -> fprintf chan "%b" b 
  | O_Str str -> fprintf chan "\"%s\"" (String.escaped str)  
  | O_Backpatch -> fprintf chan "*"  
  | O_Label i -> fprintf chan "l: %d" i 
  | O_Res -> fprintf chan "$$" 
  | O_Ret -> fprintf chan "RET" 
  | O_ByVal -> fprintf chan "V"
  | O_Entry e -> fprintf chan "%a" print_entry e 
  | O_Empty ->  fprintf chan "-"
  | O_Ref op -> fprintf chan "{%a}" print_operand op
  | O_Deref op -> fprintf chan "[%a]" print_operand op
  | O_Size i -> fprintf chan "Size %d" i
  | O_Dims i -> fprintf chan "Dims %d" i
  | O_Index lst -> fprintf chan "Indexes [%a]" print_indexes lst

(* Make quad labels consequent *)

let normalizeQuads quads =
  let map = Array.make (nextLabel()) 0 in
  let quads1 = List.mapi (fun i q -> map.(q.label) <- (i+1);
                           { label = i+1;
                             operator = q.operator;
                             arg1 = q.arg1;
                             arg2 = q.arg2;
                             arg3 = q.arg3
                           }) quads
  in
  let rec updateLabel quad = match quad.arg3 with
    | O_Label n -> quad.arg3 <- (O_Label map.(n))
    | _ -> ()
  in
  let temptbl = Hashtbl.copy labelsTbl in
  let _ = Hashtbl.clear labelsTbl in
  let () = Hashtbl.iter (fun lbl _ -> 
                                  Hashtbl.add labelsTbl map.(lbl) 0) temptbl in
    List.iter updateLabel quads1;
    quads1


let printQuad chan quad =
  fprintf chan "%d:\t %a, %a, %a, %a\n" 
    quad.label print_operator quad.operator 
    print_operand quad.arg1 print_operand quad.arg2 print_operand quad.arg3
(* A new line after Endu no longer satisfies us because it will also be
 * printed in the assembly file. *)
(*match quad.operator with
  | Q_Endu -> fprintf chan "\n"
  | _ -> ()*)

let printQuads quads = 
  List.iter (fun q -> printf "%a" printQuad q) quads

