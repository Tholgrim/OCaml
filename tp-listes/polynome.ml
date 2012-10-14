(* 2 Les polynomes  *)

    (* 2.2 Representation *)
    
let rec power x = function
    | n when n<0 -> invalid_arg "puissance negative"
    | 0 -> 1
    | n -> x * power x (n-1);;
    
let rec application poly x = match poly with
    | [] -> 0
    | (a,n)::l -> a*(power x n) + (application l x);;
    
        (* 2.3 Addition et soustraction *)
  
let rec add poly1 poly2 = match (poly1, poly2) with
        | (l, []) | ([], l) -> l
        | ((a1, n1) :: l1, (a2, n2) :: l2) ->
                match (a1 + a2, n1 - n2) with
                        | (0, 0) -> add l1 l2
                        | (_, 0) -> (a1 + a2, n1) :: (add l1 l2)
                        | (_, n) when n > 0 -> (a2, n2) :: add poly1 l2
                        | _ -> (a1, n1) :: add l1 poly2;;

let rec soustract a b = match (a, b) with
        | (l, []) -> l
        | ([], (a2, n2) :: l) -> (-a2, n2) :: (soustract [] l)
        | ((a1, n1) :: l1, (a2, n2) :: l2) ->
                match (a1 - a2, n1 - n2) with
                        | (0, 0) -> soustract l1 l2
                        | (_, 0) -> (a1 - a2, n1) :: soustract l1 l2
                        | (_, n) when n > 0 -> (-a2, n2) :: soustract a l2
                        | _ -> (a1, n1) :: soustract l1 b;;

        (* 2.4 Derivation *)

let rec deriv = function
	| [] -> []
	| (a,n)::p when n=0 -> deriv p
	| (a,n)::p -> (a*n,n-1)::(deriv p);;

