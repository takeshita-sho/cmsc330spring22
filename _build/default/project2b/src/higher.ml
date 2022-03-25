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
    fold (fun xs x -> if contains_elem xs x then [xs; 0] else [x; (count_occ lst x)]::xs) [] lst

let ap fns args = failwith "unimplemented"
