open OUnit2
open Basics

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let test_rotate _ =
  let a = String.concat "" (rotate 2 ["a"; "b"; "c"; "d"]) in 
  assert_equal ["a"; "b"; "c"; "d"] (rotate 0 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["b"; "c"; "d"; "a"] (rotate 1 ["a"; "b"; "c"; "d"]) ~msg:"rotate (2)";
  assert_equal ["c"; "d"; "a"; "b"] (rotate 2 ["a"; "b"; "c"; "d"]) ~msg:a;
  assert_equal ["d"; "a"; "b"; "c"] (rotate 3 ["a"; "b"; "c"; "d"]) ~msg:"rotate (4)";
  assert_equal ["a"; "b"; "c"; "d"] (rotate 4 ["a"; "b"; "c"; "d"]) ~msg:"rotate (5)";
  assert_equal ["b"; "c"; "d"; "a"] (rotate 5 ["a"; "b"; "c"; "d"]) ~msg:"rotate (6)";
  assert_equal [] (rotate 5 []) ~msg:"rotate (7)"

let test_get _ =
  assert_equal 26 (get 0 [26; 11; 99]) ~msg:"get (1)";
  assert_equal 11 (get 1 [26; 11; 99]) ~msg:"get (2)";
  assert_equal 99 (get 2 [26; 11; 99]) ~msg:"get (3)";
  assert_raises (Failure ("Out of bounds")) (fun () -> get 3 [26; 11; 99]) ~msg:"get (4)";
  assert_equal "a" (get 0 ["a"; "b"]) ~msg:"get (5)";
  assert_equal "b" (get 1 ["a"; "b"]) ~msg:"get (6)";
  assert_raises (Failure ("Out of bounds")) (fun () -> get 2 ["a"; "b"]) ~msg:"get (7)"

let test_larger _ =
  assert_equal [] (larger [] []) ~msg:"larger (1)";
  assert_equal [2; 3] (larger [1] [2; 3]) ~msg:"larger (2)";
  assert_equal [2; 4] (larger [2; 4] [2]) ~msg:"larger (3)";
  assert_equal [] (larger [4; 1; 2] [3; 5; 7]) ~msg:"larger (3)"
  
let test_reverse _ =
  assert_equal [] (reverse []) ~msg:"reverse (1)";
  assert_equal ["c"; "b"; "a"] (reverse ["a"; "b"; "c"]) ~msg:"reverse (2)"

let test_combine _ =
  assert_equal [] (combine [] []) ~msg:"combine (1)";
  assert_equal [3; 4] (combine [] [3; 4]) ~msg:"combine (2)";
  assert_equal [1; 2; 3; 4; 3; 4; 5] (combine [1; 2; 3; 4] [3; 4; 5]) ~msg:"combine (3)";
  assert_equal ["a"; "b"] (combine ["a"] ["b"]) ~msg:"combine (4)"

let test_palindrome _ = 
  assert_equal true (is_palindrome []) ~msg:"is_palindrome (1)";
  assert_equal true (is_palindrome ["A"; "b"; "b"; "A"]) ~msg:"is_palindrome (2)"  


let suite =
  "student" >::: [
    "rotate" >:: test_rotate;
    "get" >:: test_get;
    "larger" >:: test_larger;
    "reverse" >:: test_reverse;
    "combine" >:: test_combine;
    "palindrome" >:: test_palindrome
  ]

let _ = run_test_tt_main suite
