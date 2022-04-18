open P3.Nfa
open P3.Regexp
open TestUtils
open OUnit2

let test_move _ =
  let m1 =
    {sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, None, 2)]}
  in
  assert_nfa_move m1 [0] (Some 'a') [1] ;
  assert_nfa_move m1 [1] (Some 'a') [] ;
  assert_nfa_move m1 [2] (Some 'a') [] ;
  assert_nfa_move m1 [0;1] (Some 'a') [1] ;
  assert_nfa_move m1 [1] None [2]
let test_closure _ =
  let m4 =
    { sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, None, 2)] }
  in
  assert_nfa_closure m4 [0] [0] ;
  assert_nfa_closure m4 [1] [1;2] ;
  assert_nfa_closure m4 [0;1] [0;1;2]

let test_accept _ =
  let m1 =
    {sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)]}
  in
  assert_nfa_deny m1 "" ;
  assert_nfa_accept m1 "ac" ;
  assert_nfa_deny m1 "abc" ;
  assert_nfa_accept m1 "abac"

let test_new_states _ =
  let m1 =
  { sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)] } in
  assert_set_set_eq [ [1] ; [] ; [] ] (new_states m1 [0]) 

let test_new_trans _ =
  let m1 =
  { sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)] } in
  assert_trans_eq
    [([0; 1], Some 'a', [1]); ([0; 1], Some 'b', [0]); ([0; 1], Some 'c', [2])]
    (new_trans m1 [0;1]) 

let test_new_finals _ =
  let m1 =
  { sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)] } in
  assert_set_set_eq [[0;1;2]] (new_finals m1 [0; 1 ; 2]) ;
  assert_set_set_eq [] (new_finals m1 [0;1]) ;
  assert_set_set_eq [[0;2]] (new_finals m1 [0;2]) ;
  assert_set_set_eq [[2]] (new_finals m1 [2])

let suite =
  "student" >::: [ 
    "move" >:: test_move ;
    "closure" >:: test_closure;
    "accept" >:: test_accept;
    "new_states" >:: test_new_states;
    "new_trans" >:: test_new_trans ;
    "new_finals" >:: test_new_finals
  ]

let _ = run_test_tt_main suite
