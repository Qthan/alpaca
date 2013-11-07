open Final 
open SymbTypes
open Symbol
open Quads
open Error
open Types 

let params_size = ref 0 
let debug_codeGen = ref false

let rec codeGen quads outer = 
  let prelude = genInstr (Prelude outer) (newInstrList ()) in
    List.rev (genInstr Epilogue 
                (List.fold_left (fun lst q -> quadToFinal q lst) prelude quads))

and quadToFinal quad instr_lst = 
  let instr_lst = 
    if (memLabelTbl quad.label) 
    then genInstr (LabelDecl (makeLabel (O_Label quad.label))) instr_lst 
    else instr_lst 
  in
  let instr_lst =
    if (!debug_codeGen) then
      (
        Format.printf "%a" printQuad quad;
        let _ = Format.fprintf Format.str_formatter "%a" printQuad quad in
        let q = Format.flush_str_formatter () in
          genInstr (Comment q) instr_lst)
    else instr_lst
  in
    match quad.operator with
      | Q_Unit -> 
        let e = match quad.arg1 with 
          | O_Entry e -> e
          | _ -> internal "Must have entry."
        in
        let instr_lst1 = genInstr (Fun (makeFunctionLabel e)) instr_lst in
        let instr_lst2 = genInstr (Push (Reg Bp)) instr_lst1 in
        let instr_lst3 = genInstr (Mov (Reg Bp, Reg Sp)) instr_lst2 in
        let instr_lst4 = 
          genInstr (Sub (Reg Sp, Immediate (string_of_int (getVarSize e)))) 
            instr_lst3 
        in
          current_fun := e;
          instr_lst4
      | Q_Endu -> 
        let instr_lst1 = genInstr (Mov (Reg Sp, Reg Bp)) instr_lst in
        let instr_lst2 = genInstr (Pop (Reg Bp)) instr_lst1 in
        let instr_lst3 = genInstr Ret instr_lst2 in
        let instr_lst4 = 
          genInstr (EndFun (makeFunctionLabel !current_fun)) instr_lst3 
        in
          instr_lst4
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
      | Q_L | Q_Le | Q_G | Q_Ge | Q_Seq | Q_Nseq as relop ->
        (match (getQuadOpType quad.arg1) with
          | T_Int | T_Ref _ -> 
            let instr_lst1 = load Ax (quad.arg1) instr_lst in
            let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
            let instr_lst4 = 
              genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) 
                instr_lst3 
            in
              instr_lst4
          | T_Char | T_Bool -> 
            let instr_lst1 = load Al (quad.arg1) instr_lst in
            let instr_lst2 = load Dl (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Al, Reg Dl)) instr_lst2 in
            let instr_lst4 =
              genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) 
                instr_lst3 
            in
              instr_lst4
          | T_Float ->
            let instr_lst1 = loadReal (quad.arg1) instr_lst in
            let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr Fcompp instr_lst2 in
            let instr_lst4 = genInstr (Fstsw (Reg Ax)) instr_lst3 in
            let instr_lst5 = genInstr Sahf instr_lst4 in
            let instr_lst6 = 
              genInstr (CondJmp (relOpJmpF relop, Label (makeLabel quad.arg3))) 
                instr_lst5 
            in
              instr_lst6
          | T_Array _ -> internal "Operator = does not support type array"
          | T_Arrow _ -> internal "Operator = does not support functions"
          | T_Id _ -> 
            internal "Structual equality has been implemented elsewhere"
          | T_Unit | T_Alpha _ | T_Notype | T_Ord| T_Nofun | T_Noarr ->
            internal "Internal typed occured!"
        )
      | Q_Eq | Q_Neq as relop ->
        (match (getQuadOpType quad.arg1) with
          | T_Int | T_Ref _ | T_Id _ ->
            let instr_lst1 = load Ax (quad.arg1) instr_lst in
            let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
            let instr_lst4 = 
              genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) 
                instr_lst3 
            in
              instr_lst4
          | T_Char | T_Bool -> 
            let instr_lst1 = load Al (quad.arg1) instr_lst in
            let instr_lst2 = load Dl (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Al, Reg Dl)) instr_lst2 in
            let instr_lst4 = 
              genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) 
                instr_lst3 
            in
              instr_lst4
          | T_Float -> 
            let instr_lst1 = loadReal (quad.arg1) instr_lst in
            let instr_lst2 = loadReal (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr Fcompp instr_lst2 in
            let instr_lst4 = genInstr (Fstsw (Reg Ax)) instr_lst3 in
            let instr_lst5 = genInstr Sahf instr_lst4 in
            let instr_lst6 = 
              genInstr (CondJmp (relOpJmpF relop, Label (makeLabel quad.arg3))) 
                instr_lst5 
            in
              instr_lst6
          | T_Array _ -> internal "Operator ==,!= does not support type array"
          | T_Arrow _ -> internal "Operator ==,!= does not support functions"
          | T_Unit | T_Alpha _ | T_Notype | T_Ord| T_Nofun | T_Noarr ->
            internal "Internal typed occured!"
        )
      | Q_Dim -> 
        (*prepei na phgainei sth dieuthinsh tou pinaka (addr) 
         * kai na psaxei to megethos sto addr + (i-1)*int_size - des moodle *)
        let dim = match quad.arg2 with
          | O_Int d -> d
          | _ -> internal "Dimensions must be integers"
        in
        let instr_lst1 = load Si quad.arg1 instr_lst in
        let instr_lst2 = 
          genInstr (Mov (Reg Ax, Pointer (Word, Reg Si, word_size*(dim - 1)))) 
            instr_lst1 
        in
        let instr_lst3 = store Ax quad.arg3 instr_lst2 in
          instr_lst3
      | Q_Array -> 
        (* (e1 * (d2*d3*...dn) + e2 * (d3*...*dn) + ... en) * 
           type_size + int_size*dims
           mov di, addr
           load bx, en
           mov dx, 1  ; dx holds Πdi
           i : 
           mov ax, word ptr [di + (i-1)*int_size]
           mul dx
           mov dx, ax
           load ax, e(i-1)
           imul dx
           add bx, ax
           mov ax, type_size
           mul bx     ; ax = (e1 * (d2*d3*...dn) + 
           e2 * (d3*...*dn) + ... en) * type_size 
           add ax,2*dims  ; done
        *)
        let rec loop exprs i instr_lst = 
          match i, exprs with
            | 1, [] -> instr_lst 
            | i, e :: es ->
              let instr_lst1 = 
                genInstr (Mov (Reg Ax, Pointer (Word, Reg Di, (i-1)*word_size))) 
                  instr_lst 
              in
              let instr_lst2 = genInstr (Imul (Reg Dx)) instr_lst1 in
              let instr_lst3 = genInstr (Mov (Reg Dx, Reg Ax)) instr_lst2 in
              let instr_lst4 = load Ax e instr_lst3 in
              let instr_lst5 = genInstr (Imul (Reg Dx)) instr_lst4 in
              let instr_lst6 = genInstr (Add (Reg Bx, Reg Ax)) instr_lst5 in
                loop es (i-1) instr_lst6
            | _ -> internal "wrong arithmetic"
        in
        let (dims, type_size) = match getQuadOpType quad.arg1 with
          | T_Array (typ, D_Dim d) -> (d, sizeToBytes (getTypeSize typ))
          | T_Array (_, D_DimSize _) -> internal "Not solved dimension type"
          | _ -> internal "Only T_Array applicable"
        in
        let entry_lst = match quad.arg2 with 
          | O_Index lst -> lst
          | _ -> internal "expecting list of indices"
        in
        let instr_lst1 = load Di quad.arg1 instr_lst in
        let instr_lst2 = load Bx (List.hd entry_lst) instr_lst1 in
        let instr_lst3 = genInstr (Mov (Reg Dx, Immediate "1")) instr_lst2 in
        let instr_lst4 = loop (List.tl entry_lst) dims instr_lst3 in
        let instr_lst5 = 
          genInstr (Mov (Reg Ax, Immediate type_size)) instr_lst4 
        in
        let instr_lst6 = genInstr (Imul (Reg Bx)) instr_lst5 in
        let instr_lst7 = 
          genInstr (Add (Reg Ax, Immediate (string_of_int (word_size*dims)))) 
            instr_lst6 
        in
        let instr_lst8 = genInstr (Add (Reg Ax, Reg Di)) instr_lst7 in
        let instr_lst9 = store Ax quad.arg3 instr_lst8 in
          instr_lst9
      | Q_Assign ->
        (match getQuadOpType quad.arg1 with
          | T_Float ->
            let instr_lst1 = loadReal quad.arg1 instr_lst in
            let instr_lst2 = storeReal quad.arg3 instr_lst1 in
              instr_lst2
          | T_Arrow _ ->
            let instr_lst1 = loadFun Ax Bx quad.arg1 instr_lst in
            let instr_lst2 = storeFun Ax Bx quad.arg3 instr_lst1 in
              instr_lst2
          | ty ->
            let r1 = getRegister Ax ty in 
            let instr_lst1 = load r1 quad.arg1 instr_lst in
            let instr_lst2 = store r1 quad.arg3 instr_lst1 in
              instr_lst2)
      | Q_Ifb ->
        let instr_lst1 = load Al quad.arg1 instr_lst in
        let instr_lst2 = genInstr (Or (Reg Al, Reg Al)) instr_lst1 in
        let instr_lst3 = 
          genInstr (CondJmp ("jnz", Label (makeLabel quad.arg3))) instr_lst2 
        in
          instr_lst3
      | Q_Jump -> 
        genInstr (Jmp (Label (makeLabel quad.arg3))) instr_lst
      | Q_Jumpl | Q_Label -> internal "Redundant" 
      | Q_Call ->  
        (match quad.arg3 with
          | O_Entry e ->
            let res = functionResult e in
            let instr_lst1 = match res with
              | T_Unit ->
                let size = string_of_int word_size in 
                  genInstr (Sub (Reg Sp, Immediate size)) instr_lst
              | _ -> instr_lst
            in  
            let instr_lst2 = updateAL quad.arg3 instr_lst1 in
              (match e.entry_info with
                | ENTRY_function f ->
                  let par_size = f.function_paramsize in
                  let _  = params_size := 0 in
                  let instr_lst3 = 
                    genInstr (Call (LabelPtr (Near, makeFunctionLabel e))) 
                      instr_lst2 
                  in
                  let new_sp = (string_of_int (par_size + 2*word_size)) in
                  let instr_lst4 = 
                    genInstr (Add (Reg Sp, Immediate new_sp )) instr_lst3 
                  in
                    instr_lst4
                | ENTRY_parameter _ | ENTRY_variable _ | ENTRY_temporary _ ->
                  let instr_lst3 = loadFunCode Ax quad.arg3 instr_lst2 in
                  let par_size = !params_size in
                  let _  = params_size := 0 in
                  let instr_lst4 = genInstr (Call (Reg Ax)) instr_lst3 in
                  let new_sp = (string_of_int (par_size + 2*word_size)) in
                  let instr_lst5 = 
                    genInstr (Add (Reg Sp, Immediate new_sp)) instr_lst4 
                  in
                    instr_lst5
                | _ -> internal "Cannot call non function/parameter/variable")
          | _ -> internal "Calls can be made to entries only")
      | Q_Par ->  
        (match quad.arg2 with
          | O_Ret ->
            let instr_lst1 = loadAddress Si quad.arg1 instr_lst in
            let instr_lst2 = genInstr (Push (Reg Si)) instr_lst1 in
              instr_lst2  
          | O_ByVal ->
            let typ = getQuadOpType quad.arg1 in
            let size = getTypeSize typ in
            let _ = 
              params_size := !params_size+(int_of_string (sizeToBytes size)) in
              (match typ with
                | T_Int | T_Array _ 
                | T_Ref _ | T_Id _ ->
                  let instr_lst1 = load Ax (quad.arg1) instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                    instr_lst2                    
                | T_Float ->
                  let instr_lst1 = loadReal quad.arg1 instr_lst in
                  let instr_lst2 = 
                    genInstr (Sub (Reg Sp, Immediate (sizeToBytes TByte))) 
                      instr_lst1 
                  in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 =
                    genInstr (Fstp (Pointer (TByte, Reg Si, 0))) instr_lst3 
                  in
                    instr_lst4                    
                | T_Char | T_Bool ->
                  let instr_lst1 = load Al quad.arg1 instr_lst in
                  let instr_lst2 = 
                    genInstr (Sub (Reg Sp, Immediate (sizeToBytes Byte))) 
                      instr_lst1 
                  in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 = 
                    genInstr (Mov (Pointer (Byte, Reg Si, 0), Reg Al)) 
                      instr_lst3 
                  in
                    instr_lst4                  
                | T_Arrow (_, _) ->
                  let instr_lst1 = loadFun Ax Bx quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                  let instr_lst3 = genInstr (Push (Reg Bx)) instr_lst2 in
                    instr_lst3
                | T_Alpha _ | T_Notype | T_Ord | T_Nofun | T_Noarr -> 
                  internal "Type inference failed"
                | T_Unit -> internal "Intermediate failed")
          | _ -> internal "Only call by val and return is supported")
      | Q_Ret ->  internal "Don't have"
      | Q_Constr -> 
        let instr_lst1 = load Ax quad.arg1 instr_lst in
        let instr_lst2 = load Bx quad.arg2 instr_lst1 in
        let instr_lst3 = genInstr (Add (Reg Ax, Reg Bx)) instr_lst2 in
        let instr_lst4 = store Ax quad.arg3 instr_lst3 in
          instr_lst4
      | Q_Match ->
        let tag = getTag (entry_of_quadop quad.arg2) in
        let instr_lst1 = load Si quad.arg1 instr_lst in
        let instr_lst2 = 
          genInstr (Mov (Reg Ax, Pointer (Word, Reg Si, 0))) instr_lst1 in
        let instr_lst3 = 
          genInstr (Cmp (Reg Ax, Immediate (string_of_int tag))) instr_lst2 in
        let instr_lst4 = 
          genInstr (CondJmp ("je", Label (makeLabel quad.arg3))) instr_lst3 
        in
          instr_lst4 
      | Q_Fail ->
        let instr_lst1 = 
          genInstr (Mov (Reg Ax, Immediate "4C01h")) instr_lst in
        let instr_lst2 = genInstr (Interrupt (Immediate "21h")) instr_lst1 in
          instr_lst2

