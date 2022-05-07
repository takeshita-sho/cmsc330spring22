open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let tokenize input =
  let re_string = Str.regexp "\"[^\"]*\"" in
  let re_bool = Str.regexp "true\\|false" in
  let re_negint = Str.regexp "(-[0-9]+)" in
  let re_int = Str.regexp "[0-9]+" in 
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let re_eq = Str.regexp "=" in
  let re_neq = Str.regexp "<>" in
  let re_great = Str.regexp ">" in
  let re_less = Str.regexp "<" in
  let re_ge = Str.regexp ">=" in
  let re_le = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "not" in
  let re_if = Str.regexp "if" in
  let re_then = Str.regexp "then" in
  let re_else = Str.regexp "else" in
  let re_add = Str.regexp "+" in
  let re_sub = Str.regexp "-" in
  let re_mult = Str.regexp "*" in
  let re_div = Str.regexp "/" in
  let re_concat = Str.regexp "\\^" in
  let re_let = Str.regexp "let" in
  let re_def = Str.regexp "def" in
  let re_in = Str.regexp "in" in
  let re_rec = Str.regexp "rec" in
  let re_fun = Str.regexp "fun" in
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let re_more = Str.regexp "[a-zA-Z0-9]+" in
  let re_arrow = Str.regexp "->" in
  let re_double = Str.regexp ";;" in
  let re_sp = Str.regexp "[ \t\n]+" in
  
  let rec tok pos s = 
    if pos >= String.length s then
      []
    else
      if (Str.string_match re_string s pos) then
        let t = Str.matched_string s in
        let token = String.sub t 1 ((String.length t)-2) in
          (Tok_String token)::(tok (pos+String.length(t)) s)
      else if (Str.string_match re_bool s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          (Tok_Bool (bool_of_string token))::(tok (pos+String.length(token)) s)
      else if (Str.string_match re_negint s pos) then
        let t = Str.matched_string s in
        let token = String.sub t 1 ((String.length t)-2) in
          (Tok_Int (int_of_string token))::(tok (pos+String.length(t)) s)
      else if (Str.string_match re_int s pos) then
        let token = Str.matched_string s in
          (Tok_Int (int_of_string token))::(tok (pos+String.length(token)) s)   

      else if (Str.string_match re_lparen s pos) then
        Tok_LParen::(tok (pos+1) s)
      else if (Str.string_match re_rparen s pos) then
        Tok_RParen::(tok (pos+1) s)
      else if (Str.string_match re_eq s pos) then
        Tok_Equal::(tok (pos+1) s)

      else if (Str.string_match re_arrow s pos) then
        Tok_Arrow::(tok (pos+2) s)
      else if (Str.string_match re_neq s pos) then
        Tok_NotEqual::(tok (pos+2) s)

      else if (Str.string_match re_ge s pos) then
        Tok_GreaterEqual::(tok (pos+2) s)
      else if (Str.string_match re_le s pos) then
        Tok_LessEqual::(tok (pos+2) s)
      else if (Str.string_match re_great s pos) then
        Tok_Greater::(tok (pos+1) s)
      else if (Str.string_match re_less s pos) then
        Tok_Less::(tok (pos+1) s)
      else if (Str.string_match re_or s pos) then
        Tok_Or::(tok (pos+2) s)
      else if (Str.string_match re_and s pos) then
        Tok_And::(tok (pos+2) s)

      else if (Str.string_match re_not s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Not::(tok (pos+3) s)
      else if (Str.string_match re_if s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_If::(tok (pos+2) s)
      else if (Str.string_match re_then s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Then::(tok (pos+4) s)
      else if (Str.string_match re_else s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Else::(tok (pos+4) s)

      else if (Str.string_match re_add s pos) then
        Tok_Add::(tok (pos+1) s)
      else if (Str.string_match re_sub s pos) then
        Tok_Sub::(tok (pos+1) s)
      else if (Str.string_match re_mult s pos) then
        Tok_Mult::(tok (pos+1) s)
      else if (Str.string_match re_div s pos) then
        Tok_Div::(tok (pos+1) s)
      else if (Str.string_match re_concat s pos) then
        Tok_Concat::(tok (pos+1) s)

      else if (Str.string_match re_let s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Let::(tok (pos+3) s)
      else if (Str.string_match re_def s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Def::(tok (pos+3) s)
      else if (Str.string_match re_in s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_In::(tok (pos+2) s)
      else if (Str.string_match re_rec s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
          Tok_Rec::(tok (pos+3) s)
      else if (Str.string_match re_fun s pos) then
        let token = Str.matched_string s in
        let next = Str.match_end() in 
        if (Str.string_match re_more s next) then
          let token2 = Str.matched_string s in
          (Tok_ID (token^token2))::(tok (Str.match_end()) s)
        else 
        Tok_Fun::(tok (pos+3) s)
      else if (Str.string_match re_id s pos) then
        let token = Str.matched_string s in
          (Tok_ID token)::(tok (pos+String.length(token)) s)
      else if (Str.string_match re_double s pos) then
        Tok_DoubleSemi::(tok (pos+2) s)
      else if (Str.string_match re_sp s pos) then
        let token = Str.matched_string s in
          tok (pos+String.length(token)) s
      else
        raise (InvalidInputException ("Invalid Input"))
  in tok 0 input