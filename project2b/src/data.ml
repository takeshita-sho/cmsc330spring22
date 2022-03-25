open Funs
open Higher

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

(*gotta check if x is already in tree*)
let rec int_insert x t = match t with 
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (a,None,c,d,e) -> if x > a then IntNode (a, Some x,c,d,e) else if x=a then IntNode(a,None,c,d,e) else IntNode (x,Some a,c,d,e)
  | IntNode (a,Some b,c,d,e) -> if x < a then IntNode (a,Some b,(int_insert x c),d,e) else if x > b then IntNode (a,Some b,c,d,(int_insert x e)) else
  if x=a||x=b then IntNode(a,Some b,c,d,e) else IntNode (a,Some b,c,(int_insert x d),e)

let rec int_mem x t = match t with
  | IntLeaf -> false
  | IntNode (a,None,c,d,e) -> if x=a then true else false
  | IntNode (a,Some b,c,d,e) -> if x=a||x=b then true else if x<a then int_mem x c else if x>b then int_mem x e else int_mem x d

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode(a,None,c,d,e) -> 1
  | IntNode(a,b,c,d,e) -> 2+(int_size c)+(int_size d)+(int_size e)
  

let rec int_max t = match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode(a,None,c,d,e) -> a
  | IntNode(a,Some b,c,d,IntLeaf) -> b
  | IntNode(a,Some b,c,d,e) -> int_max e

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
  | MapLeaf -> MapNode ((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a,b),None,d,e,f) -> if k > a then MapNode ((a,b), Some (k,v),d,e,f) else if k=a then raise (Invalid_argument("map_put")) else MapNode ((k,v),Some (a,b),d,e,f)
  | MapNode ((a,b),Some (c,d),e,f,g) -> if k < a then MapNode ((a,b),Some (c,d),(map_put k v e),f,g) else if k > c then MapNode ((a,b),Some (c,d),e,f,(map_put k v g)) else
  if k=a||k=c then raise (Invalid_argument("map_put")) else MapNode ((a,b),Some (c,d),e,(map_put k v f),g)

let rec map_contains k t = match t with
  | MapLeaf -> false
  | MapNode ((a,b),None,d,e,f) -> if k=a then true else false
  | MapNode ((a,b),Some (c,d),e,f,g) -> if k=a||k=c then true else if k<a then map_contains k e else if k>c then map_contains k g else map_contains k f

let rec map_get k t = match t with
  | MapLeaf -> raise (Invalid_argument("map_get"))
  | MapNode ((a,b),None,c,d,e) -> if a=k then b else raise (Invalid_argument("map_get"))
  | MapNode ((a,b),Some (c,d),e,f,g) -> if a=k then b else if c=k then d else if k<a then map_get k e else if k>c then map_get k g else map_get k f
  

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = match table with
  | [] -> [[]]
  | x::xs -> []::x::xs


let pop_scope (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "No scopes remain!"
  | x::xs -> xs

let rec add_aux k v l = match l with
  | [] -> [(k,v)]
  | (a,b)::xs -> if a=k then failwith "Duplicate variable binding in scope!" else [(a,b)]@(add_aux k v xs)

let add_var name value (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | x::xs -> (add_aux name value x)::xs

let rec look_aux k l = match l with
  | [] -> None
  | (a,b)::xs -> if a=k then Some b else look_aux k xs

let to_int x = match x with
  | None -> 0
  | Some a -> a

let rec lookup name (table : lookup_table) = match table with
  | [] -> failwith "Variable not found!"
  | x::xs -> let t = look_aux name x in if t=None then lookup name xs else to_int t
  