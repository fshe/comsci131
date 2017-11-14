(* Name: Frank She

   UID: 204172020

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(str), x) -> Env.add_binding str x (Env.empty_env())
    | (TuplePat(mpLst), TupleVal(mvLst)) -> 
        (match (mpLst, mvLst) with 
          ([],[]) -> Env.empty_env()
          | (h1::t1, h2::t2) -> Env.combine_envs (patMatch h1 h2) (patMatch (TuplePat(t1)) (TupleVal(t2)))
        )
    | (DataPat(str1, mv1), DataVal(str2, mv2)) when str1=str2 ->
      (match (mv1, mv2) with 
        (Some v1, Some v2) -> (patMatch v1 v2)
        | (None, None) -> Env.empty_env()
      )
    | _ -> raise MatchFailure

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
    | BoolConst(i) -> BoolVal(i)
    | Var(str) -> 
      (try
        (Env.lookup str env)
      with 
         Env.NotBound -> raise (DynamicTypeError ("Error: Unbound value "^str))
      )
    | BinOp(e1, op, e2) -> 
      (match ((evalExpr e1 env), op, (evalExpr e2 env)) with
         (IntVal(x), Plus, IntVal(y)) -> IntVal(x+y)
          | (IntVal(x), Minus, IntVal(y)) -> IntVal(x-y)
          | (IntVal(x), Times, IntVal(y)) -> IntVal(x*y)
          | (IntVal(x), Eq, IntVal(y)) -> if (x = y) then BoolVal(true) else BoolVal(false)
          | (IntVal(x), Gt, IntVal(y)) -> if (x > y) then BoolVal(true) else BoolVal(false)
          | _ -> raise (DynamicTypeError "Binary operations can only be performed on integer values")
      )
    | Negate(e1) -> 
        (match e1 with 
          IntConst(x) -> IntVal(-x)
          | Var(str) -> 
          (match (Env.lookup str env) with
                IntVal(x) -> IntVal(-x)
                | _ -> raise (DynamicTypeError "Error: expected type of int")
          )
          | _ -> raise (DynamicTypeError "Error: This expression expected type int")
        )
    | If(e1, e2, e3) -> 
        (match e1 with 
          BoolConst(b) -> if b then evalExpr e2 env else evalExpr e3 env
          | Var(str) -> 
            (try
              (match (Env.lookup str env) with
                BoolVal(b) -> if b then evalExpr e2 env else evalExpr e3 env
                | _ -> raise (DynamicTypeError "Error: This expression expected type bool")
              )
            with
              Env.NotBound -> raise (ImplementMe ("Error: Unbound value "^str))
            )
          | BinOp(a, op, c) -> 
              (match (evalExpr (BinOp(a, op, c)) env) with
                BoolVal(b) -> if b then evalExpr e2 env else evalExpr e3 env
                | _ -> raise (DynamicTypeError "Error: This expression expected type bool")
              )
        )
    | Function(p, e1) -> FunctionVal(None, p, e1, env)

    (*function call is the function name and the arguments *)
    | FunctionCall(e1, e2) -> 
        (let functionHead = (evalExpr e1 env) in 
          (match functionHead with 
            FunctionVal(None, fpattern, fexpression, fenvironment) -> 
              let functionBody = (evalExpr e2 env) in
              let funcEnv = (patMatch fpattern functionBody) in
              let newEnv = (Env.combine_envs fenvironment funcEnv) in 
              (evalExpr fexpression newEnv)

            | FunctionVal(Some str, fpattern, fexpression, fenvironment) -> 
              let functionBody = (evalExpr e2 env) in
              let funcEnv = (patMatch fpattern functionBody) in
              let newEnv = (Env.combine_envs fenvironment funcEnv) in 
              (* Need to add a binding for its own name *)
              (evalExpr fexpression (Env.add_binding str functionHead newEnv) )
          )
        )
    | Match(me1, mpmeLst) -> 
      let value = (evalExpr me1 env) in 
      (match mpmeLst with 
        [] -> raise MatchFailure
        | (mp, me)::t -> 
          (try
            (evalExpr me (Env.combine_envs (patMatch mp value) env))
           with  
            MatchFailure -> (evalExpr (Match(me1, t)) env)
          )   
      )
    | Tuple(mxLst) -> 
        TupleVal( List.map (function x -> evalExpr x env) mxLst )
    | Data(str, moption) -> 
        (match moption with
          None -> DataVal(str, None)
          | Some v -> DataVal(str, Some(evalExpr v env))
        )

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | Let(str, moExpr) -> (Some str, evalExpr moExpr env)
    | LetRec(str, moExpr) -> 
      match moExpr with
        Function(p, e1) -> (Some str, FunctionVal(Some str, p, e1, env))
        | FunctionCall(e1, e2) ->
            match (evalExpr e1 env) with
              FunctionVal(_, fpattern, fexpression, fenvironment) -> 
                (Some str, FunctionVal(Some str, fpattern, fexpression, fenvironment))









