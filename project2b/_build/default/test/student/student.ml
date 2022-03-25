open OUnit2
open P2b.Data
open P2b.Funs
open P2b.Higher
  

let test_insert _ = 
  let t0 = int_insert 5 IntLeaf in
  let t1 = int_insert 5 t0 in
  let t2 = int_insert 1 t1 in
  let t3 = int_insert 6 t2 in
  let t4 = int_insert 0 t3 in

  assert(t0 = IntNode (5, None, IntLeaf, IntLeaf, IntLeaf));
  assert(t1 = IntNode (5, None, IntLeaf, IntLeaf, IntLeaf));
  assert(t2 = IntNode (1, Some 5, IntLeaf, IntLeaf, IntLeaf));
  assert(t3 = IntNode (1, Some 5, IntLeaf, IntLeaf, IntNode(6, None, IntLeaf, IntLeaf, IntLeaf)));
  assert(t4 = IntNode (1, Some 5, IntNode(0, None, IntLeaf, IntLeaf, IntLeaf), IntLeaf, IntNode(6, None, IntLeaf, IntLeaf, IntLeaf)));;

let suite =
  "student" >::: [
    "insert" >:: test_insert
  ]

let _ = run_test_tt_main suite
