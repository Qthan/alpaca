open Final 
open SymbTypes
open Symbol
open Quads
open Error
open Types 

let params_size = ref 0 
let debug_codeGen = ref true

let rec codeGen quads outer = 
  let prelude = genInstr (Prelude outer) (newInstrList ()) in
    List.rev (genInstr Epilogue (List.fold_left (fun lst q -> quadToFinal q lst) prelude quads))

and quadToFinal quad instr_lst = 
  let instr_lst = 
    if (memLabelTbl quad.label) 
    then genInstr (LabelDecl (makeLabel (O_Label quad.label))) instr_lst 
    else instr_lst 
  in
  let instr_lst =
    if (!debug_codeGen) then 
      (Format.printf "%a" printQuad quad;
       genInstr (Comment (Format.sprintf "%a" printQuad quad)))
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
        let instr_lst4 = genInstr (Sub (Reg Sp, Immediate (string_of_int (getVarSize e)))) instr_lst3 in
          current_fun := e;
          instr_lst4
      | Q_Endu -> 
        let instr_lst1 = genInstr (Mov (Reg Sp, Reg Bp)) instr_lst in
        let instr_lst2 = genInstr (Pop (Reg Bp)) instr_lst1 in
        let instr_lst3 = genInstr Ret instr_lst2 in
        let instr_lst4 = genInstr (EndFun (makeFunctionLabel !current_fun)) instr_lst3 in
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
      | Q_Pow -> instr_lst (* Dummy value *)
      | Q_L | Q_Le | Q_G | Q_Ge as relop ->
        (match (getQuadOpType quad.arg1) with
          | T_Int | T_Char -> 
            let instr_lst1 = load Ax (quad.arg1) instr_lst in
            let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
            let instr_lst4 = genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) instr_lst3 in
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
      | Q_Seq | Q_Nseq as relop ->
        (match (getQuadOpType quad.arg1) with
          | T_Int | T_Char | T_Bool | T_Ref _ ->
            let instr_lst1 = load Ax (quad.arg1) instr_lst in
            let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
            let instr_lst4 = genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) instr_lst3 in
              instr_lst4
          | T_Float -> internal "not yet implemented"
          | T_Array _ -> internal "Operator = does not support type array"
          | T_Arrow _ -> internal "Operator = does not support functions"
          | T_Id _ -> internal "fuck :)")
      | Q_Eq | Q_Neq as relop ->
        (match (getQuadOpType quad.arg1) with
          | T_Int | T_Char | T_Bool | T_Ref _ ->
            let instr_lst1 = load Ax (quad.arg1) instr_lst in
            let instr_lst2 = load Dx (quad.arg2) instr_lst1 in
            let instr_lst3 = genInstr (Cmp (Reg Ax, Reg Dx)) instr_lst2 in
            let instr_lst4 = genInstr (CondJmp (relOpJmp relop, Label (makeLabel quad.arg3))) instr_lst3 in
              instr_lst4
          | T_Float -> internal "not yet implemented"
          | T_Array _ -> internal "Operator = does not support type array"
          | T_Arrow _ -> internal "Operator = does not support functions"
          | T_Id _ -> internal "a different kind of fuck :)")
      | Q_Dim -> 
        (*prepei na phgainei sth dieuthinsh tou pinaka (addr) kai na psaxei to megethos sto addr + (i-1)*int_size - des moodle *)
        let dim = match quad.arg2 with
          | O_Int d -> d
          | _ -> internal "Dimensions must be integers"
        in
        let instr_lst1 = load Ax quad.arg1 instr_lst in
        let instr_lst2 = genInstr (Mov (Reg Ax, Pointer (Word, Reg Ax, word_size*(dim - 1)))) instr_lst1 in
        let instr_lst3 = store Ax quad.arg3 instr_lst2 in
          instr_lst3
      (* old version
         let instr_lst1 = load Ax (quad.arg2 - 1) instr_lst in
         let instr_lst2 = genInstr (Mov (Reg Cx, word_size)) instr_lst1 in
         let instr_lst3 = genInstr (Imul (Reg Cx)) instr_lst2 in
         let instr_lst4 = loadAddr Cx quad.arg1 instr_lst3 in
         let instr_lst5 = genInstr (Add (Reg Ax, Reg Cx)) instr_lst4 in
         let instr_lst6 = genInstr (Mov (Reg Ax, Pointer (Word, Reg Ax, 0))) instr_lst5 in
         let instr_lst7 = store Ax quad.arg3 instr_lst6 in
         instr_lst7 *)
      | Q_Array -> 
        (* pairneis ton arithmo twn diastasew n tou pinaka, kai gia na vreis to i-osto stoixeio
         * pas addr+ int_size + i*size *)
        let (dims_offset, typ) = match getQuadOpType quad.arg1 with
          | T_Array (typ, D_Int d) -> (d*word_size, typ)
          | T_Array (_, D_Alpha _) -> internal "Not solved dimention type"
          | T_Ref typ -> (0, typ)
        in
        let instr_lst1 = load Ax quad.arg2 instr_lst in
        let instr_lst2 = genInstr (Mov (Reg Cx, Immediate (sizeToBytes (getTypeSize typ)))) instr_lst1 in
        let instr_lst3 = genInstr (Imul (Reg Cx)) instr_lst2 in
        let instr_lst4 = genInstr (Add (Reg Ax, Immediate (string_of_int dims_offset))) instr_lst3 in
        let instr_lst5 = load Cx quad.arg1 instr_lst4 in
        let instr_lst6 = genInstr (Add (Reg Ax, Reg Cx)) instr_lst5 in
        let instr_lst7 = store Ax quad.arg3 instr_lst6 in
          instr_lst7
      | Q_Assign ->  
        (match getQuadOpType quad.arg1 with
          | T_Float ->
            let instr_lst1 = loadReal quad.arg1 instr_lst in
            let instr_lst2 = storeReal quad.arg2 instr_lst1 in
              instr_lst2
          | T_Arrow _ ->
            let instr_lst1 = loadFun Ax Bx quad.arg1 instr_lst in
            let instr_lst2 = storeFun Ax Bx quad.arg3 instr_lst1 in
              instr_lst2
          | _ ->
            let instr_lst1 = load Ax quad.arg1 instr_lst in
            let instr_lst2 = store Ax quad.arg3 instr_lst1 in
              instr_lst2)
      | Q_Ifb ->
        let instr_lst1 = load Al quad.arg1 instr_lst in
        let instr_lst2 = genInstr (Or (Reg Al, Reg Al)) instr_lst1 in
        let instr_lst3 = genInstr (CondJmp ("jnz", Label (makeLabel quad.arg3))) instr_lst2 in
          instr_lst3
      | Q_Jump -> 
        genInstr (Jmp (Label (makeLabel quad.arg3))) instr_lst
      | Q_Jumpl | Q_Label -> internal "Redundant" 
      | Q_Call ->  
        (match quad.arg3 with
          | O_Entry e ->
            let res = functionResult e in
            let instr_lst1 = match res with
              | T_Unit -> genInstr (Sub (Reg Sp, Immediate (string_of_int word_size))) instr_lst
              | _ -> instr_lst
            in  
            let instr_lst2 = updateAL e instr_lst1 in
              (match e.entry_info with
                | ENTRY_function f ->
                  let par_size = f.function_paramsize in
                  let instr_lst3 = genInstr (Call (LabelPtr (Near, makeFunctionLabel e))) instr_lst2 in
                  let instr_lst4 = genInstr (Add (Reg Sp, Immediate (string_of_int (par_size + 2*word_size)))) instr_lst3 in
                    instr_lst4
                | ENTRY_parameter _ | ENTRY_variable _ | ENTRY_temporary _ -> (* temporary is REDUNDANT *)
                  let par_size = !params_size in
                  let _  = params_size := 0 in
                  let offset = getOffset e in
                  let instr_lst3 = genInstr (Mov (Reg Bx, Pointer(Word, Reg Bp, offset))) instr_lst2 in
                  let instr_lst4 = genInstr (Call (Reg Bx)) instr_lst3 in
                  let instr_lst5 = genInstr (Add (Reg Sp, Immediate (string_of_int (par_size + 2*word_size)))) instr_lst4 in
                    instr_lst5
                | _ -> internal "Cannot call non function/parameter/variable"))
      | Q_Par ->  
        (match quad.arg2 with
          | O_Ret ->
            let size = Word in
            let _ = params_size := !params_size + (int_of_string (sizeToBytes size)) in
            let instr_lst1 = loadAddress Si quad.arg1 instr_lst in
            let instr_lst2 = genInstr (Push (Reg Si)) instr_lst1 in
              instr_lst2  
          | O_ByVal ->
            let typ = getQuadOpType quad.arg1 in
            let size = getTypeSize typ in
            let _ = params_size := !params_size + (int_of_string (sizeToBytes size)) in
              (match typ with
                | T_Int ->
                  let _ = params_size := !params_size + (int_of_string (sizeToBytes Word)) in
                  let instr_lst1 = load Ax (quad.arg1) instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                    instr_lst2                    
                | T_Float ->
                  let _ = params_size := !params_size + (int_of_string (sizeToBytes TByte)) in
                  let instr_lst1 = loadReal quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes TByte))) instr_lst1 in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 = genInstr (Fstp (Pointer (TByte, Reg Si, 0))) instr_lst3 in
                    instr_lst4                    
                | T_Char | T_Bool ->
                  let _ = params_size := !params_size + (int_of_string (sizeToBytes Byte)) in
                  let instr_lst1 = load Al quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes Byte))) instr_lst1 in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 = genInstr (Mov (Pointer (Byte, Reg Si, 0), Reg Al)) instr_lst3 in
                    instr_lst4                    
                | T_Array _ | T_Ref _ ->
                  let instr_lst1 = load Ax (quad.arg1) instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                    instr_lst2
                | T_Arrow (_, _) ->
                  let instr_lst1 = loadFun Ax Bx quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                  let instr_lst3 = genInstr (Push (Reg Bx)) instr_lst2 in
                    instr_lst3
                | T_Id _ -> internal "Not (YET) implemented"    
                | T_Alpha _  | T_Notype | T_Ord | T_Nofun-> internal "Type inference failed"
                | T_Unit -> internal "Intermediate failed"
                | T_Str -> internal "T_Str is redundant. GET OVER IT."))
      | Q_Ret ->  internal "Don't have"



(*(match quad.arg1 with
      | O_Entry e ->
        let size = getSize e in
        let _ = params_size := !params_size + (int_of_string (sizeToBytes size)) in
          (match quad.arg2 with
            | O_ByVal ->
              (match size with
                | Word -> 
                  let instr_lst1 = load Ax quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                    instr_lst2
                | Byte ->
                  let instr_lst1 = load Al quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes Byte))) instr_lst1 in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 = genInstr (Mov (Pointer (Byte, Reg Si, 0), Reg Al)) instr_lst3 in
                    instr_lst4
                | DWord ->
                  let instr_lst1 = loadFun Ax Bx quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
                  let instr_lst3 = genInstr (Push (Reg Bx)) instr_lst2 in
                    instr_lst3
                | TByte -> 
                  let instr_lst1 = loadReal quad.arg1 instr_lst in
                  let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes TByte))) instr_lst1 in
                  let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
                  let instr_lst4 = genInstr (Fstp (Pointer (TByte, Reg Si, 0))) instr_lst3 in
                    instr_lst4
              )
            | O_Ret ->
              let instr_lst1 = loadAddress Si quad.arg1 instr_lst in
              let instr_lst2 = genInstr (Push (Reg Si)) instr_lst1 in
                instr_lst2)
      | O_Int _  | O_Deref _ -> 
        let _ = params_size := !params_size + (int_of_string (sizeToBytes Word)) in
        let instr_lst1 = load Ax (quad.arg1) instr_lst in
        let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
          instr_lst2
      | O_Bool _ | O_Char _ ->
        let _ = params_size := !params_size + (int_of_string (sizeToBytes Byte)) in
        let instr_lst1 = load Al quad.arg1 instr_lst in
        let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes Byte))) instr_lst1 in
        let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
        let instr_lst4 = genInstr (Mov (Pointer (Byte, Reg Si, 0), Reg Al)) instr_lst3 in
          instr_lst4
      | O_Float _ ->
        let _ = params_size := !params_size + (int_of_string (sizeToBytes TByte)) in
        let instr_lst1 = loadReal quad.arg1 instr_lst in
        let instr_lst2 = genInstr (Sub (Reg Sp, Immediate (sizeToBytes TByte))) instr_lst1 in
        let instr_lst3 = genInstr (Mov (Reg Si, Reg Sp)) instr_lst2 in
        let instr_lst4 = genInstr (Fstp (Pointer (TByte, Reg Si, 0))) instr_lst3 in
          instr_lst4
      | O_Str _ ->
        let _ = params_size := !params_size + (int_of_string (sizeToBytes Word)) in
        let instr_lst1 = loadAddress Ax (quad.arg1) instr_lst in
        let instr_lst2 = genInstr (Push (Reg Ax)) instr_lst1 in
          instr_lst2
      | O_Backpatch -> internal "Cannot push backpatch"
      | O_Label _ -> internal "Cannot push label"
      | O_Res -> internal "Cannot push res"
      | O_Ret -> internal "Cannot push ret"
      | O_ByVal -> internal "Cannot push byval"
      | O_Empty -> internal "Cannot push empty"
      | O_Ref _ -> internal "Cannot push ref"
      (*| O_Deref _ -> internal "Cannot push deref"*)
      | O_Size _ -> internal "Cannot push size"
      | O_Dims _ -> internal "Cannot push dims"
    )*)

