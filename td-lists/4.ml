(* 4. Il y a plusieurs listes *)

	(* 4.1 Représentation des polynômes par listes *)

		(* 1 *)

let poly = [(4,5); (1,2); (-3,1); (1,0)];;
let rec find_coef n = function
	| [] -> n
	| (x,y)::l -> if y = n then x else find_coef (n-1) l;;