open Final

(* Some but not all are listed below :
   1. mov si, bp
    push si
   to push bp 
   2. 
    mov ax, const
    mov di, word ptr [off]
    mov word ptr [di], ax
   to 
    mov di, word ptr [off]
    mov word ptr [di], const
   3. 
    mov ax, const
    mov word ptr [off], ax
   to
    mov word ptr[off], const 
   4.
    mov ax, word ptr [off]
    mov dx, word ptr [off2]
    cmp ax, dx
   to
    mov ax, word ptr [off]
    cmp ax, word ptr [off2] 
   5.
    mov ax, word ptr [off1]
    mov dx, const
    add ax, dx
    mov word ptr [off2], ax
   to 
    if [off1] = [off2] then
      add word ptr [off1], const - done by first peep2 and then peep3.
    else simplify mov and add.
   6. 
    mov ax, val
    op target, ax - target is register.
   to 
    op target, val
   7. 
    mov ax, 0
    mov dx, val
    sub ax, val
   to
    mov ax, val
    neg ax 
   8.
    mov mem1, r1
    mov r2, mem1
   to
    mov mem1, r1
    mov r2, r1
   9.
    mov mem1, r1
    mov r1, mem1
   to
    mov mem1, r1
   10. 
    jcond target1
    jmp target2
   to 
    jrevcond target2 
*)

let rec peep3 instructions acc =
  match instructions with
      [] -> List.rev acc
    | (Mov (Reg Ax, Immediate "0")) :: (Mov (Reg Dx, value1))
      :: (Sub (Reg Ax, value2)) :: is when value1 = value2 ->
      peep3 is ((Neg (Reg Ax)) :: (Mov (Reg Ax, value2)) :: acc)
    | (Mov (Reg r1, (Pointer (size, op, off) as p1))) 
      :: (Add (Reg r2, Immediate s))
      :: (Mov ((Pointer (size2, op2, off2) as p2), Reg r3))
      :: is when r1 = r2 && r2 = r3 && p1 = p2 ->
      peep3 is ((Add (p1, Immediate s)) :: acc)
    | (Mov (Reg r1, Immediate s)) 
      :: (Mov (Reg r2, pointer))
      :: (Mov (Pointer (size2, Reg r3, off2), Reg r4)) 
      :: is when r1 = r4 && r2 = r3 ->
      let instr1 = (Mov (Pointer (size2, Reg r3, off2), Immediate s)) in
      let instr2 = (Mov (Reg r2, pointer)) in
        peep3 is (instr1 :: instr2 :: acc)
    | (CondJmp (cond, Label target)) :: (Jmp target2) 
      :: (LabelDecl l) :: is when target = l ->
      let reverted = Final.revertCond cond in
        peep3 is ((LabelDecl l) :: (CondJmp (reverted, target2)) :: acc)
    | instr :: is ->
      peep3 is (instr :: acc)

let rec peep2 instructions acc = 
  match instructions with
      [] -> List.rev acc
    | (Mov (Reg Si, Reg Bp)) :: (Push (Reg Si)) :: is ->
      peep2 is ((Push (Reg Bp)) :: acc)
    | (Mov (Reg r1, Immediate s)) :: (Mov ((Pointer (size,op,off)), Reg r2)) 
      :: is when r1 = r2 ->
      peep2 is ((Mov ((Pointer (size, op, off)), Immediate s)) :: acc)
    | (Mov (Reg r1, value)) :: (Add (Reg target, Reg r2)) :: is when r1 = r2 ->
      peep2 is ((Add (Reg target, value)) :: acc)
    | (Mov (Reg r1, value)) :: (Sub (Reg target, Reg r2)) :: is when r1 = r2 ->
      peep2 is ((Sub (Reg target, value)) :: acc)
    | (Mov (Reg r1, value)) :: (Cmp (Reg z, Reg r2)) :: is when r1 = r2 ->
      peep2 is ((Cmp (Reg z, value)) :: acc)
    | (Mov (Reg Ax, Immediate "0")) :: (Sub (Reg Ax, value)) :: is ->
      peep2 is ((Neg (Reg Ax)) :: (Mov (Reg Ax, value)) :: acc)
    | (Mov (p1, Reg r1)) :: (Mov (Reg r2, p2)) :: is when p1 = p2 && r1 = r2 ->
      peep2 is ((Mov (p1, Reg r1)) :: acc)
    | (Mov (p1, Reg r1)) :: (Mov (Reg r2, p2)) :: is when p1 = p2 && r1 <> r2 ->
      peep2 is ((Mov (Reg r2, Reg r1)) :: (Mov (p1, Reg r1)) :: acc)
    | instr :: is -> peep2 is (instr :: acc)

let rec peep1 instructions acc =
  match instructions with
      [] -> List.rev acc
    | (Add (target, Immediate "1")) :: is ->
      peep1 is ((Inc target) :: acc)
    | (Add (target, Immediate "0")) :: is ->
      peep1 is acc
    | (Sub (target, Immediate "1")) :: is ->
      peep1 is ((Dec target) :: acc)
    | (Sub (target, Immediate "0")) :: is ->
      peep1 is acc
    | instr :: is ->
      peep1 is (instr :: acc)

(*make multiple passes*)
let optimize instr =
  let optimized_instr = peep3 instr [] in
  let optimized_instr = peep2 optimized_instr [] in
  let optimized_instr = peep3 optimized_instr [] in
  let optimized_instr = peep2 optimized_instr [] in
  let optimized_instr = peep1 optimized_instr [] in
  let optimized_instr = peep2 optimized_instr [] in
    optimized_instr
