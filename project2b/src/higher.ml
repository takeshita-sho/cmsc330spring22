open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    if fold (fun counter x -> if x = e then counter+1 else counter) 0 lst > 0 then true else false

let is_present lst x = 
    rev (fold (fun l z -> if (z = x) then 1::l else 0::l) [] lst)

let count_occ lst target = 
    fold (fun counter x -> if x = target then counter+1 else counter) 0 lst

let uniq lst = 
    rev (fold (fun xs x -> if contains_elem xs x then xs else x::xs) [] lst)

let assoc_list lst = 
    let unique = uniq lst in
    let f xs x = xs@[(x, count_occ lst x)] in
    fold f [] unique

(* Applies each function in fns to each argument in args in order, collecting all results in a single list. *)
let ap fns args = 
    fold (fun xs x -> xs@(map x args)) [] fns
