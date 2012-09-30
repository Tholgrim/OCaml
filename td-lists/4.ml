(* 4. Il y a plusieurs listes *)

	(* 4.1 Représentation des polynômes par listes *)

		(* 1 *)

let poly = [(4,5); (1,2); (-3,1); (1,0)];;
let rec find_coef n = function
	| [] -> 0
	| (x,y)::l -> if y = n then x else find_coef n l;;

		(* 2 *)

let poly1 = [(4,5); (1,2); (-3,1); (1,0)];;
let poly2 = [(2,5); (2,4); (-1,2); (1,1)];;

let rec poly_addition a b = match (a,b) with
	| ([],[]) -> []
	| ((x,y)::l,[]) | ([],(x,y)::l) -> (x,y)::(poly_addition l [])
	| ((x1,y1)::l1,(x2,y2)::l2) -> 
		if y1=y2 then
			if x1+x2 = 0 then (poly_addition l1 l2) else (x1+x2,y1)::(poly_addition l1 l2)
		else
			if y1>y2 then (x1,y1)::(poly_addition l1 ((x2,y2)::l2))
			else (x2,y2)::(poly_addition ((x1,y1)::l1) l2);;