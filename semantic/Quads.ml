open Types

let label = ref 0 

let newLabel = fun () -> incr label; !label

let nextLabel = fun () -> !label + 1

let newTemp =
  let k = ref 0 in
    fun typ -> incr k; O_Temp (!k, typ)

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


let newQuadList () = []
let newLabelList () = []
let makeLabelList n = [n]

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

let setExprInfo p n = { place = p; next = n }

let setCondInfo t f = { true_lst = t; false_lst = f } 
