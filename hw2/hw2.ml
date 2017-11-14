
exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

let (vplus : vector -> vector -> vector) =
  fun v1 v2 -> List.map2 (+.) v1 v2;;

let (mplus : matrix -> matrix -> matrix) =
  fun m1 m2 -> List.map2 vplus m1 m2;;

let (dotprod : vector -> vector -> float) =
  fun v1 v2 -> List.fold_right (+.) (List.map2 ( *. ) v1 v2) 0.0;;

let (transpose : matrix -> matrix) =
  fun m ->
    let arrayIndices = match m with [] -> [] | h::t -> List.mapi (fun idx e -> idx) h
  in
    let reusableVar = 
    List.sort (fun (idx, e) (idx2, e) -> if (idx = idx2) then 0 else if (idx > idx2) then 1 else -1) 
    (
      List.flatten 
      (
        List.map (fun v -> List.mapi (fun idx e -> (idx, e)) v) m
      )
    )
    in

    List.map (fun x -> List.map (fun y -> match y with (idx, b) -> b) x) 
        (List.map (fun idx -> List.filter (fun (b, _) -> if (b = idx) then true else false) reusableVar) arrayIndices)
  ;;

let (mmult : matrix -> matrix -> matrix) =
  fun m1 m2 -> 
    (List.map (fun v1 -> (List.map (fun v2 -> (dotprod v1 v2) ) (transpose m2) )) m1)
  ;;
        
(* Problem 2: Calculators *)           
           
(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let rec (evalExp : exp -> float) =
  fun e ->
    match e with 
      Num(f) -> f
      | BinOp(f1, operator, f2) -> 
      let evalf1 = (evalExp f1) in
      let evalf2 = (evalExp f2) in
        match operator with
          Plus -> evalf1 +. evalf2
          | Minus -> evalf1 -. evalf2
          | Times -> evalf1 *. evalf2
          | Divide -> evalf1 /. evalf2
  ;;

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

let (execute : instr list -> float) =
  fun instrList -> 
    let rec aux = fun floatList instrList ->
      match instrList with
      [] -> if (List.length floatList > 0) then List.hd floatList else 0.
      | h::t -> 
        (match h with
        Push(f) -> (aux (f::floatList) t )
        | Calculate(o) -> 
          (match floatList with
            [] -> 0. (*error*)
            | f1::f2::t2 -> aux (evalExp (BinOp(Num(f2), o, Num(f1)))::t2)  t)
        | Swap -> 
          (match floatList with
            [] -> (aux floatList t)
            | f1::f2::t2 -> (aux (f2::f1::t2) t)))
    in
      (aux [] instrList)
    ;;

let rec (compile : exp -> instr list) =
  fun e ->
    match e with
      Num(f) -> [Push(f)]
      | BinOp(left, o, right) -> (compile left)@(compile right)@[Calculate(o)]
  ;;

let (decompile : instr list -> exp) =
  fun instrList -> 
    let rec aux = fun expList instrList ->
        match instrList with
        [] -> expList
         | h::t -> 
           match h with
              Swap -> aux expList t
              | Push(f) -> aux (Num(f)::expList) t
              | Calculate(o) -> 
                (match expList with
                  [] -> aux expList t
                  | h1::h2::t2 -> aux (BinOp(h2, o, h1)::t2) t
                )
    in
     List.hd (aux [] instrList)
  ;;


(* EXTRA CREDIT *)        
let (compileOpt : exp -> (instr list * int)) =
  raise ImplementMe

