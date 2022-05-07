open OUnit2
open TestUtils
open P4a.Lexer
open P4a.MicroCamlTypes

let test_tokenize1 _ =
    let result = tokenize "\"\"\"" in
    assert_equal result [Tok_Int 1; Tok_Add; Tok_Int 2]



let suite = 
  "public" >::: [
    "test_tokenize1" >:: test_tokenize1
  ]

let _ = run_test_tt_main suite