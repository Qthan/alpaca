open Format
open Types

let rec pretty_dim ppf dim1 =
  match dim1 with 
    | D_Int n -> fprintf ppf "%d" n
    | D_Alpha n -> fprintf ppf "d@@%d" n
    
let rec pretty_typ ppf typ =
  match typ with
    | T_Unit -> 
        fprintf ppf "unit"
    | T_Int ->
        fprintf ppf "int"
    | T_Char ->
        fprintf ppf "char"
    | T_Bool ->
        fprintf ppf "bool"
    | T_Str ->
        fprintf ppf "string"
    | T_Float ->
        fprintf ppf "float"
    | T_Notype ->
        fprintf ppf "undefined"
    | T_Ord -> 
        fprintf ppf "ord"
    | T_Nofun ->
        fprintf ppf "nofun"
    | T_Arrow (a,b) ->
        fprintf ppf "(%a -> %a)" pretty_typ a pretty_typ b
    | T_Ref a -> 
        fprintf ppf "%a ref" pretty_typ  a
    | T_Array (a,n) -> 
        fprintf ppf "array (%a, %a)" pretty_typ a pretty_dim n
    | T_Id str ->
        fprintf ppf "Udt %s" (str)
    | T_Alpha a ->
        fprintf ppf "@@%d" a



let print_solved lst = 
  let rec pp_solved ppf solved = 
    let pp_tuple ppf (t1, t2) =
      fprintf ppf "(%a, %a)" pretty_typ t1 pretty_typ t2 
    in
      match solved with 
        | [] -> ()
        | x::[] -> fprintf ppf "%a" pp_tuple x
        | x::xs -> fprintf ppf "%a, %a" pp_tuple x pp_solved xs
  in
    printf "%a" pp_solved lst
