open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  List.fold_left (fun acc x -> match x with a, b, c -> if s = b && elem a qs && not (elem c acc) then c::acc else acc) [] nfa.delta
  
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let r = qs in
  let rec aux r qs =
    let qs = r in let r = union qs (List.fold_left (fun acc x -> match x with a, b, c -> if None = b && elem a qs && not (elem c acc) then c::acc else acc) [] nfa.delta) in if qs = r then r else aux r qs
  in aux r qs

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let rec aux str res =
    match str with
    | h::t -> aux t (e_closure nfa (move nfa res(*e_closure nfa res*) (Some h)))
    | [] -> if eq (diff res nfa.fs) res then false else true 
  in aux (explode s) (e_closure nfa [nfa.q0])(*[nfa.q0]*)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc x -> (e_closure nfa (move nfa qs(*e_closure nfa qs*) (Some x)))::acc) [] nfa.sigma (*Maybe remove first closure *)

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc x -> (qs, Some x, (e_closure nfa (move nfa qs (Some x))))::acc) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let rec aux lst =
    match lst with
    | h::t -> if elem h nfa.fs then [qs] else aux t
    | [] -> []
  in aux qs

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  | h::t -> let e = new_states nfa h in nfa_to_dfa_step nfa {sigma=nfa.sigma;qs=(union e dfa.qs);q0=dfa.q0;fs=if eq (new_finals nfa h) [] then dfa.fs else (union [h] dfa.fs);delta=(union (new_trans nfa h) dfa.delta)} (union t (List.fold_left (fun acc x-> if elem x dfa.qs then acc else x::acc) [] e)) 
  | [] -> dfa

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let r0 = e_closure nfa [nfa.q0] in
  nfa_to_dfa_step nfa {sigma=nfa.sigma;qs=[r0];q0=r0;fs=[];delta=[]} [r0]
