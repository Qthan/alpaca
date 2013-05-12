open Format
open Types
open AstTypes
open Identifier
open Error
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

type quad_operators =
  | Q_Unit | Q_Endu
  | Q_Plus | Q_Minus | Q_Mult | Q_Div | Q_Mod
  | Q_Fplus | Q_Fminus | Q_Fmult | Q_Fdiv | Q_Pow
  | Q_L | Q_Le | Q_G | Q_Ge | Q_Seq | Q_Nseq
  | Q_Eq | Q_Neq (* Physical equality *)
  | Q_Assign | Q_Ifb | Q_Array
  | Q_Jump | Q_Jumpl | Q_Label
  | Q_Call | Q_Par | Q_Ret | Q_Dim

type quad_operands = 
  | O_Int of int
  | O_Float of float
  | O_Char of string
  | O_Bool of bool
  | O_Str of string 
  | O_Backpatch
  | O_Label of int
  | O_Temp of int * typ
  | O_Res (* $$ *)
  | O_Ret (* RET *)
  | O_ByVal
  | O_Fun of string
  | O_Obj of string * typ 
  | O_Empty
  | O_Ref of quad_operands
  | O_Deref of quad_operands
  | O_Size of int
  | O_Dims of int

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

let newTemp =
  let k = ref 0 in
  fun typ -> incr k; O_Temp (!k, typ)

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
  | Power -> Q_Pow
  | Seq -> Q_Seq 
  | Nseq -> Q_Nseq
  | L -> Q_L
  | Le -> Q_Le
  | G -> Q_G
  | Ge -> Q_Ge
  | Eq -> Q_Eq
  | Neq -> Q_Neq
  | And | Or | Semicolon -> internal "no operator for and/or/;" 
  | Assign -> Q_Assign

let getQuadUnop unop = match unop with
  | U_Plus -> Q_Plus
  | U_Minus -> Q_Minus
  | U_Fplus -> Q_Fplus
  | U_Fminus -> Q_Fminus
  | U_Not | U_Del -> internal "no operator for not/delete"

(* XXX Backpatch, changes a mutable field so we can maybe avoid returning a new
 * quad list thus avoiding all the quads1,2,3... pollution. Moo XXX*)
let backpatch quads lst patch =
  List.iter (fun quad_label -> 
      match (try Some (List.find (fun q -> q.label = quad_label) quads) with Not_found -> None) with
        | None -> internal "Quad label not found, can't backpatch\n"
        | Some quad -> quad.arg3 <- O_Label patch) lst;
  quads

let newQuadList () = []

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
  | Q_Pow -> "**"
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


let print_operator chan op = fprintf chan "%s" (string_of_operator op)


let rec print_operand chan op = match op with
  | O_Int i -> fprintf chan "%d" i 
  | O_Float f -> fprintf chan "%f" f 
  | O_Char str -> fprintf chan "\'%s\'" str 
  | O_Bool b -> fprintf chan "%b" b 
  | O_Str str -> fprintf chan "\"%s\"" str  
  | O_Backpatch -> fprintf chan "*"  
  | O_Label i -> fprintf chan "l: %d" i 
  | O_Temp (i, t) -> fprintf chan "temp[$%d, %a]" i pretty_typ t
  | O_Res -> fprintf chan "$$" 
  | O_Ret -> fprintf chan "RET" 
  | O_ByVal -> fprintf chan "V"
  | O_Fun n -> fprintf chan "fun[%s]" n 
  | O_Obj (n, t) -> fprintf chan "Obj[%s, %a]" n pretty_typ t
  | O_Empty ->  fprintf chan "-"
  | O_Ref op -> fprintf chan "{%a}" print_operand op
  | O_Deref op -> fprintf chan "[%a]" print_operand op
  | O_Size i -> fprintf chan "Size %d" i
  | O_Dims i -> fprintf chan "Dims %d" i

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
    List.iter updateLabel quads1;
    quads1


let printQuad chan quad =
  fprintf chan "%d:\t %a, %a, %a, %a\n" 
    quad.label print_operator quad.operator 
    print_operand quad.arg1 print_operand quad.arg2 print_operand quad.arg3;
  match quad.operator with
    | Q_Endu -> fprintf chan "\n"
    | _ -> ()

let printQuads quads = 
  List.iter (fun q -> printf "%a" printQuad q) quads
