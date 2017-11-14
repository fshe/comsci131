
exception ImplementMe

(* Problem 1 *)
            
let rec (member : 'a -> 'a list -> bool) =
  fun x lst ->
	match lst with
	[] -> false
	| h1::t when h1=x -> true
	| h1::t -> member x t;;

let (add : 'a -> 'a list -> 'a list) =
  fun x s -> 
	match s with 
	[] -> [x]
	| h1::t when (not (member x s)) -> x::h1::t
	| _ -> s;;

let rec (union : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
	match s1 with 
	[] -> s2
	| h1::t -> union t (add h1 s2);;

let rec (fastUnion : 'a list -> 'a list -> 'a list) =
	fun s1 s2 ->
		match s1 with
		[] -> s2
		| h1::t1 -> 
			match s2 with
			[] -> s1
			| h2::t2 when (h2 > h1) -> 
				(add h1 (fastUnion t1 s2))
			| h2::t2 -> add h2 (fastUnion s1 t2);;
let (intersection : 'a list -> 'a list -> 'a list) =
	fun s1 s2 -> 
		List.filter (fun x -> (member x s2)) s1;;
 
let rec (setify : 'a list -> 'a list) =
  fun l -> 
	match l with 
	[] -> []
	| h::t -> let set = setify(t) in 
	if (member h t) then set else h::set;;


let rec help = fun x s ->
	match s with
	[] -> [[]]
	| [[]] -> [[x]]
	| h::t -> [x::h]@[h]@(help x t)
	;;
let rec (powerset : 'a list -> 'a list list) =
	fun s ->
	match s with 
	[] -> [[]]
	| h::t -> (help h (powerset t))
	;;

			
        
(* Problem 2 *)        
        
let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
	fun f l -> 
	match l with
	[] -> [],[]
	| h::t -> 
		match (partition f t) with
		(l1, l2) -> if (f h) then h::l1, l2 else l1, h::l2;;
		

let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
	fun p f v ->
		match (p v) with
		true -> whle (p) (f) (f v)
		| false -> v;;
                                    
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
	fun x f ->
		match x with
		0 -> (function y -> y)
		| 1 -> f
		| _ -> (function y ->  (pow (x-1) (f)) (f y));;
