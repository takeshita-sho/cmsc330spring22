open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Value a -> (match a with
    | Int i -> Int i
    | Bool b -> Bool b
    | String str -> String str
    | Closure (envir,var,expr) -> Closure (envir,var,expr)
    )
  | ID id -> (lookup env id)
  | Not expr -> let v = eval_expr env expr in
    (match v with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Not bool")
    )
  | Binop (op, exp1, exp2) -> (match op with
    | Add -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Int (a+b)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Sub -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Int (a-b)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Mult -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Int (a*b)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Div -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> if b = 0 then raise (DivByZeroError) else Int (a/b)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Greater -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Bool (e1>e2)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Less -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Bool (e1<e2)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | GreaterEqual -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Bool (e1>=e2)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | LessEqual -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int a -> (match e2 with
        | Int b -> Bool (e1<=e2)
        | _ -> raise (TypeError "Expected Int")
        )
      | _ -> raise (TypeError "Expected Int") 
      )
    | Concat -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | String a -> (match e2 with
        | String b -> String (a^b)
        | _ -> raise (TypeError "Expected String")
        )
      | _ -> raise (TypeError "Expected String") 
      )
    | Equal -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int i -> (match e2 with 
        | Int a -> Bool (a=i)
        | _ -> raise (TypeError "Cannot compare types"))
      | Bool b -> (match e2 with
        | Bool a -> Bool (a=b)
        | _ -> raise (TypeError "Cannot compare types"))
      | String a -> (match e2 with
        | String b -> Bool (a=b)
        | _ -> raise (TypeError "Cannot compare types"))
      | _ -> raise (TypeError "Cannot compare types") 
      )
    | NotEqual -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with 
      | Int i -> (match e2 with 
        | Int a -> Bool (a<>i)
        | _ -> raise (TypeError "Cannot compare types"))
      | Bool b -> (match e2 with
        | Bool a -> Bool (a<>b)
        | _ -> raise (TypeError "Cannot compare types"))
      | String a -> (match e2 with
        | String b -> Bool (a<>b)
        | _ -> raise (TypeError "Cannot compare types"))
      | _ -> raise (TypeError "Cannot compare types") 
      )
    | Or -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with
      | Bool a -> (match e2 with
        | Bool b -> Bool (a||b)
        | _ -> raise (TypeError "Expected Bool"))
      | _ -> raise (TypeError "Expected Bool"))
    | And -> let e1 = eval_expr env exp1 in let e2 = eval_expr env exp2 in
      (match e1 with
      | Bool a -> (match e2 with
        | Bool b -> Bool (a&&b)
        | _ -> raise (TypeError "Expected Bool"))
      | _ -> raise (TypeError "Expected Bool"))
    )
  | If (exp1,exp2,exp3) -> let e1 = eval_expr env exp1 in
    (match e1 with
    | Bool a -> if a then eval_expr env exp2 else eval_expr env exp3
    | _ -> raise (TypeError "Not Bool"))
  | Let (var, b, exp1,exp2) -> if b then let env1 = (extend_tmp env var) in let e1 = eval_expr env1 exp1 in 
    (update env1 var e1);eval_expr env1 exp2 else 
    let e1 = eval_expr env exp1 in
    eval_expr (extend env var e1) exp2
  | Fun (var,exp) -> Closure(env,var,exp)
  | FunctionCall (exp1,exp2) -> let e1 = eval_expr env exp1 in
    (match e1 with
    | Closure (envir,var,exp3) -> let e2 = eval_expr env exp2 in eval_expr (extend envir var e2) exp3
    | _ -> raise (TypeError "Not a Function"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def (var,exp) -> let env1 = extend_tmp env var in let e1 = eval_expr env1 exp in 
    update env1 var e1; (env1,(Some e1))

  | Expr exp -> let e = eval_expr env exp in (env,(Some e))
  | NoOp -> (env, None)