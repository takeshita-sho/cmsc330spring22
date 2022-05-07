open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks =  
  match (lookahead toks) with 
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks 
  | Some Tok_Fun -> parse_fun toks 
  | _ -> parse_or toks 

and parse_let toks =
  match (lookahead toks) with
  | Some Tok_Let -> let t = match_token toks (Tok_Let) in 
    let (t',recur) = parse_recursion t in
     let (t'',id) = parse_id t' in
     let tok_eq = match_token (t'') (Tok_Equal) in
     let (t''',exp1) = parse_expr tok_eq in
     let tok_in = match_token (t''') (Tok_In) in
     let (t'''',exp2) = parse_expr tok_in in
    (t'''', Let (id,recur,exp1,exp2))
  | _ -> raise (InvalidInputException "parse_let wrong")

and parse_if toks = 
  let iflst = match_token toks Tok_If in
  let (t,m1) = parse_expr iflst in
  let thenlst = match_token t Tok_Then in
  let (t',m2) = parse_expr thenlst in
  let elselst = match_token t' Tok_Else in
  let (t'',m3) = parse_expr elselst in
  (t'',If(m1,m2,m3))

and parse_fun toks = 
  let funlst = match_token toks Tok_Fun in
  let (t,m1) = parse_id funlst in
  let arrlst = match_token t Tok_Arrow in
  let (t',m2) = parse_expr arrlst in
  (t',Fun(m1,m2))

and parse_recursion toks =
  match lookahead toks with
  | Some Tok_Rec -> ((match_token toks (Tok_Rec)), true)
  | _ -> (toks, false)

and parse_id toks =
  match (lookahead toks) with
  | Some Tok_ID str -> ((match_token toks (Tok_ID str)),str)
  | _ -> raise (InvalidInputException "parse_id wrong")

and parse_or toks =
  let (t,m1) = parse_and toks in
  match lookahead t with
  | Some Tok_Or -> let orlst = match_token t Tok_Or in
    let (t',m2) = parse_or orlst in
    (t',Binop(Or,m1,m2))
  | _ -> (t,m1)

and parse_and toks =
  let (t,m1) = parse_equality toks in
  match lookahead t with
  | Some Tok_And -> let andlst = match_token t Tok_And in
    let (t',m2) = parse_and andlst in
    (t',Binop(And,m1,m2))
  | _ -> (t,m1)

and parse_equality toks =
  let (t,m1) = parse_rel toks in
  match lookahead t with
  | Some (Tok_Equal|Tok_NotEqual) -> let (t',m2) = parse_eqop t in
    let (t'',m3) = parse_equality t' in
    (t'', Binop (m2,m1,m3))
  | _ -> (t,m1)

and parse_eqop toks =
  match lookahead toks with
  | Some Tok_Equal -> ((match_token toks Tok_Equal),Equal)
  | Some Tok_NotEqual -> ((match_token toks Tok_NotEqual),NotEqual)
  | _ -> raise (InvalidInputException "parse_eqop")

and parse_rel toks = 
  let (t,m1) = parse_add toks in
  match lookahead t with
  | Some (Tok_Less|Tok_Greater|Tok_LessEqual|Tok_GreaterEqual) -> let (t',m2) = parse_relop t in
    let (t'',m3) = parse_rel t' in
    (t'', Binop (m2,m1,m3))
  | _ -> (t,m1)

and parse_relop toks =
  match lookahead toks with
  | Some Tok_Less -> ((match_token toks Tok_Less), Less)
  | Some Tok_Greater -> ((match_token toks Tok_Greater), Greater)
  | Some Tok_LessEqual -> ((match_token toks Tok_LessEqual), LessEqual)
  | Some Tok_GreaterEqual -> ((match_token toks Tok_GreaterEqual), GreaterEqual)
  | _ -> raise (InvalidInputException "parse_relop")

and parse_add toks =
  let (t,m1) = parse_mul toks in
  match lookahead t with
  | Some (Tok_Add|Tok_Sub) -> let (t',m2) = parse_addop t in
    let (t'',m3) = parse_add t' in
    (t'', Binop(m2,m1,m3))
  | _ -> (t,m1)

and parse_addop toks =
  match lookahead toks with
  | Some Tok_Add -> ((match_token toks Tok_Add), Add)
  | Some Tok_Sub -> ((match_token toks Tok_Sub), Sub)
  | _ -> raise (InvalidInputException "addop")

and parse_mul toks =
  let (t,m1) = parse_concat toks in
  match lookahead t with
  | Some (Tok_Mult|Tok_Div) -> let (t',m2) = parse_mulop t in
    let (t'',m3) = parse_mul t' in
    (t'',Binop(m2,m1,m3)) 
  | _ -> (t,m1)

and parse_mulop toks =
  match lookahead toks with
  | Some Tok_Mult -> ((match_token toks Tok_Mult), Mult)
  | Some Tok_Div -> ((match_token toks Tok_Div), Div)
  | _ -> raise (InvalidInputException "parse_mulop")

and parse_concat toks =
  let (t,m1) = parse_unary toks in
  match lookahead t with
  | Some Tok_Concat -> let conlst = match_token t Tok_Concat in
    let (t',m2) = parse_concat conlst in
    (t',Binop(Concat,m1,m2))
  | _ -> (t,m1)

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not -> let notlst = match_token toks Tok_Not in
    let (t,m) = parse_unary notlst in
    (t,(Not m)) (*fix later *)
  | _ -> parse_funcall toks

and parse_funcall toks =
  let (t,m1) = parse_primary toks in
  match lookahead t with
  | Some Tok_Int i -> let (t',m2) = parse_primary t in
    (t',FunctionCall(m1,m2))
  | Some Tok_Bool i -> let (t',m2) = parse_primary t in
    (t',FunctionCall(m1,m2))
  | Some Tok_String i -> let (t',m2) = parse_primary t in
    (t',FunctionCall(m1,m2))
  | Some Tok_ID i -> let (t',m2) = parse_primary t in
    (t',FunctionCall(m1,m2))
  | Some (Tok_LParen) -> let (t',m2) = parse_primary t in
    (t',FunctionCall(m1,m2))
  | _ -> (t,m1)

and parse_primary toks =
  match lookahead toks with
  | Some Tok_Int i -> ((match_token toks (Tok_Int i)), Value (Int i))
  | Some Tok_Bool b -> ((match_token toks (Tok_Bool b)), Value (Bool b))
  | Some Tok_String str -> ((match_token toks (Tok_String str)), Value (String str))
  | Some Tok_ID id -> ((match_token toks (Tok_ID id)),(ID id))
  | Some Tok_LParen -> let t = match_token toks Tok_LParen in 
    let (t',m) = parse_expr t in
    let t'' = match_token t' Tok_RParen in
    (t'',m)
  | _ -> raise (InvalidInputException "parse_primary")

let rec parse_mutop toks =  
  match lookahead toks with
  | Some Tok_Def -> let deflst = match_token toks Tok_Def in
    let (t,m1) = parse_id deflst in
    let eqlst = match_token t Tok_Equal in
    let (t',m2) = parse_expr eqlst in
    let semilst = match_token t' Tok_DoubleSemi in
    if semilst <> [] then
      raise (InvalidInputException "mutop def")
    else
      (semilst,Def(m1,m2))
  | Some Tok_DoubleSemi -> let lst = match_token toks Tok_DoubleSemi in
    if lst <> [] then
      raise (InvalidInputException "mutop semi")
    else
      (lst,NoOp)
  | _ -> let (t,m1) = parse_expr toks in
    let lst = match_token t Tok_DoubleSemi in
    if lst <> [] then
      raise (InvalidInputException "mutop expr")
    else 
      (lst,Expr(m1))