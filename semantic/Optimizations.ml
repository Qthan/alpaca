open Cfg
open SymbTypes
open Error
open Quads

(** Common Sub-expression elimination *)
let cse cfg = 
  let optimized_cfg = 
    CFG.persistent_vmap Cse.simulate cfg
  in
   optimized_cfg

let removable_temp q q1 =
  Quads.isBop q.operator && q.arg3 = q1.arg1 && q1.operator = Q_Assign

let remove_temps (info, s, b) =
  let rec aux b acc =
    match b with
        [] ->
          let f = match Blocks.(info.cur_fun) with
              None -> internal "I dropped the ball"
            | Some e ->  
                (match e.entry_info with
                     ENTRY_function f -> f
                   | _ -> internal "Nothing else applies")
          in
          let () = Symbol.fixTmpOffsets f in
            (info, s, Blocks.rev acc)
      | q :: q1 :: qs when removable_temp q q1 ->
          let tmp_e = Quads.deep_entry_of_quadop q.arg3 in
          let (Some e_f) = Blocks.(info.cur_fun) in
          let () = Symbol.remove_temp e_f tmp_e in
          let () = q.arg3 <- q1.arg3 in
            aux qs (q :: acc)
      | q :: qs -> aux qs (q :: acc)
  in
   aux b [] 
            

let simplify cfg =
  let no_temps = CFG.persistent_vmap remove_temps cfg in
   no_temps 

let optimize cfg =
  let simplified = simplify cfg in
  let cse_cfg = cse simplified in
    cse_cfg
