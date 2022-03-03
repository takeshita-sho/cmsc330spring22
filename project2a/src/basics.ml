(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    match tup with
    (a, b, c) -> (c, b, a)

let is_odd x =
    if x mod 2 = 0 then false
    else true

let area x y = 
    let (a,b) = x in
    let (c,d) = y in
    abs ((a-c)*(b-d));; 


let volume x y = 
    let (a,b,c) = x in
    let (d,e,f) = y in
    abs ((a-d)*(b-e)*(c-f));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    if n = 0 then 0 else if n < 3 then 1 else fibonacci(n-1) + fibonacci(n-2)

let rec pow x y = 
    if y = 0 then 1 else x * pow x (y-1)

let rec log x y = 
    if x > y then 0 else
    1 + log x (y/x)

let rec gcf x y = 
    if y = 0 then x else
    gcf y (x mod y)

let rec prime_aux x y =
    if x <= 2 then if x = 2 then true else false else
    if (x mod y) = 0 then false else
    if (y*y) > x then true else
    prime_aux x (y+1) 

let rec is_prime x = 
    prime_aux x 2


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
    match lst with
    | [] -> failwith "Out of bounds"
    | x::xs -> if idx = 0 then x else get (idx-1) xs

let rec length lst i =
    match lst with
    | [] -> i
    | x::xs -> length xs (i+1)

let larger lst1 lst2 = 
    let i = length lst1 0 in
    let j = length lst2 0 in
    if i > j then lst1 else if i < j then lst2 else []

let rec append l m = match l with
    | [] -> m
    | (x::xs) -> x::(append xs m)


let rec reverse lst = match lst with 
    | [] -> []
    | (x::xs) -> append (reverse xs) (x::[])

let rec combine lst1 lst2 = 
    append lst1 lst2

let rec insert x lst = match lst with
    | [] -> [x]
    | h::t -> if x > h then h::insert x t else x::lst

let rec sort lst = match lst with
    | [] -> []
    | h::t -> insert h (sort t)

let rec merge lst1 lst2 = 
    let lst = append lst1 lst2 in
    sort lst
    
let rec div lst n =
    match lst with
    | [] -> lst
    | h::t -> if n = 0 then lst else div (t@[h]) (n-1)

let rec rotate shift lst = 
    let l = length lst 0 in
    let n = if l = 0 then 0 else (shift mod l) in
    if n = 0 then lst else 
    div lst n

let rec pal_aux lst i =
    if i > ((length lst 0)/2) then true else
    if get i lst = get ((length lst 0)-1-i) lst then pal_aux lst (i+1) else false

let rec is_palindrome lst = 
    if (length lst 0) = 0 then true else pal_aux lst 0
    