open Cfg

(** Common Sub-expression elimination *)
let cse cfg = 
  let optimized_cfg = 
    CFG.persistent_vmap Cse.simulate cfg
  in
   optimized_cfg


let optimize cfg = 
  let cse_cfg = cse cfg in
    cse_cfg
