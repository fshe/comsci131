

let rec (member : 'a -> 'a list -> bool) =
  fun x s ->
  match s with
    [] -> false
  | h::t when h=x -> true
  | h::t -> member x t

let (add : 'a -> 'a list -> 'a list) =
  fun x s ->
  if member x s then s else x::s

let rec (union : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  match s1 with
    [] -> s2
  | h::t -> add h (union t s2)

(* assuming the input sets are in sorted order *)                
let rec (fastUnion : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  match (s1, s2) with
    ([], _) -> s2
  | (_, []) -> s1
  | (h1::t1, h2::t2) when h1 < h2 -> add h1 (fastUnion t1 s2)
  | (h1::t1, h2::t2) -> add h2 (fastUnion s1 t2)
                
let (intersection : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  List.filter (function x -> member x s2) s1
                
let rec (setify : 'a list -> 'a list) =
  function l ->
  match l with
    [] -> []
  | h::t -> add h (setify t)
         
let rec (powerset : 'a list -> 'a list list) =
  fun s ->
  match s with
    [] -> [[]]
  | h::t ->
     let p1 = powerset t in
     let p2 = List.map (function s -> h::s) p1 in
     p1@p2


(* like filter, but it returns a pair of lists *)          
let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
  fun p l ->
  match l with
    [] -> ([], [])
  | h::t ->
     let (l1, l2) = partition p t in
     if p h then (h::l1, l2) else (l1, h::l2)


(* a while loop *)                     
let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
  fun p f x ->
  if p x then whle p f (f x) else x

                                    
(* raise f to the nth power *)                                    
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
  fun n f ->
  match n with
    0 -> (function x -> x)
  | _ ->
     let f' = pow (n-1) f in
     function x -> f (f' x)

                     