open Final 
open Symbol 

let quadToFinal quad instr_lst = 
  match quad.operator with
  | Q_Unit -> 
    let entry = match quad.arg1 with 
      | O_Entry e -> e
      | _ -> internal "Must have entry."
    in
    let instr_lst1 = genInstr (Fun (makeFunctionLabel e)) instr_lst in
    let instr_lst2 = genInstr (Push (Reg Bp)) instr_lst1 in
    let instr_lst3 = genInstr (Mov (Reg Bp, Reg Sp)) instr_lst2 in
    let instr_lst4 = genInstr (Sub (Reg Sp, Immediate (string_of_int (getVarSize e)))) instr_lst3 in
      current_fun := e;
      instrLst4
  | Q_Endu -> 
    let instr_lst1 = genInstr (Mov (Reg Sp, Reg Bp)) instr_lst in
    let instr_lst2 = genInstr (Pop (Reg Bp)) instr_lst1 in
    let instr_lst3 = genInstr Ret instr_lst2 in
    let instr_lst4 = genInstr (EndFun (makeFunctionLabel !current_fun)) instr_lst3 in
      instrLst4
  | Q_Plus -> 
    let instr_lst1 = load Ax (quad.arg1) instr_lst in
    let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Add (Reg Ax, Reg Dx)) instr_lst2 in
    let instr_lst4 = store Ax (quad.arg3) instr_lst3 in
      instr_lst4
  | Q_Minus ->
    let instr_lst1 = load Ax (quad.arg1) instr_lst in
    let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Sub (Reg Ax, Reg Dx)) instr_lst2 in
    let instr_lst4 = store Ax (quad.arg3) instr_lst3 in
      instr_lst4
  | Q_Mult ->
    let instr_lst1 = load Ax (quad.arg1) instr_lst in
    let instr_lst2 = load Cx (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Imul (Reg Cx)) instr_lst2 in
    let instr_lst4 = store Ax (quad.arg3) instr_lst3 in
      instr_lst4
  | Q_Div ->
    let instr_lst1 = load Ax (quad.arg1) instr_lst in
    let instr_lst2 = genInstr Cwd instr_lst1 in
    let instr_lst3 = load Cx (quad.arg2) instr_lst2 in
    let instr_lst4 = genInstr (Idiv (Reg Cx)) instr_lst3 in
    let instr_lst5 = store Ax (quad.arg3) instr_lst4 in
      instr_lst5
  | Q_Mod -> 
    let instr_lst1 = load Ax (quad.arg1) instr_lst in
    let instr_lst2 = genInstr Cwd instr_lst1 in
    let instr_lst3 = load Cx (quad.arg2) instr_lst2 in
    let instr_lst4 = genInstr (Idiv (Reg Cx)) instr_lst3 in
    let instr_lst5 = store Dx (quad.arg3) instr_lst4 in
      instr_lst5
  | Q_Fplus -> 
    let instr_lst1 = loadReal (quad.arg1) instr_lst in
    let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Faddp (Reg (ST 1), Reg (ST 0))) instr_lst2 in
    let instr_lst4 = storeReal (quad.arg3) instr_lst3 in
      instr_lst4    
  | Q_Fminus ->  
    let instr_lst1 = loadReal (quad.arg1) instr_lst in
    let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Fsubp (Reg (ST 1), Reg (ST 0))) instr_lst2 in
    let instr_lst4 = storeReal (quad.arg3) instr_lst3 in
      instr_lst4    
  | Q_Fmult -> 
    let instr_lst1 = loadReal (quad.arg1) instr_lst in
    let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Fmulp (Reg (ST 1), Reg (ST 0))) instr_lst2 in
    let instr_lst4 = storeReal (quad.arg3) instr_lst3 in
      instr_lst4  
  | Q_Fdiv ->  
    let instr_lst1 = loadReal (quad.arg1) instr_lst in
    let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
    let instr_lst3 = genInstr (Fdivp (Reg (ST 1), Reg (ST 0))) instr_lst2 in
    let instr_lst4 = storeReal (quad.arg3) instr_lst3 in
      instr_lst4  
  | Q_Pow -> instr_lst (* Dummy value *)
  | Q_L | Q_Le | Q_G | Q_Ge as relop ->
    (match (getQuadOpType quad.arg1) with
      | T_Int | T_Char -> 
        let instr_lst1 = load Ax (quad.arg1) instr_lst in
        let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
        let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
        let instr_lst4 = genInstr (CondJmp (relOpJmp relop, makeLabel quad.arg3)) instr_lst3 in
          instr_lst4
      | T_Float ->
        (*let instr_lst1 = loadReal (quad.arg1) instr_lst in
        let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
        let instr_lst3 = genInstr Fcompp instr_lst2 in
        let instr_lst4 = genInstr (Fstsw (Reg Ax)) instr_lst3 in
        let instr_lst5 = genInstr
        let instr_lst4 = storeReal (quad.arg3) instr_lst3 in*)
        (*value kai instr apo pinaka 9.1...*)
        instr_lst (* XXX DUMMY! *)
        )
  | Q_Seq | Q_Nseq -> 
    
  | Q_Eq -> 
  | Q_Neq -> 
  | Q_Dim -> 
  | Q_Assign ->  
  | Q_Ifb ->  
  | Q_Array -> 
  | Q_Jump ->  
  | Q_Jumpl ->  
  | Q_Label -> 
  | Q_Call ->  
  | Q_Par ->  
  | Q_Ret ->  
