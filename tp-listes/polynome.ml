(* 2 Les polynomes  *)

    (* 2.2 Representation *)
    
let rec power x = function
    | n when n<0 -> invalid_arg "puissance negative"
    | 0 -> 0
    | n -> x * power x (n-1);;
    
let rec application poly x = match poly with
    | [] -> 0
    | (a,n)::l -> a*(power x n) + (application l x);;
    
    (* 2.3 Addition et soustraction *)
    
let rec add a b = match (a,b) with
    | (l,[]) | ([],l) -> l
    | ((x1,y1)::l1,(x2,y2)::l2) ->
        if y1=y2 then
            if x1+x2 = 0 then
                (add l1 l2)
            else
                (x1+x2,y1)::(add l1 l2)
        else
            if y1>y2 then
                (x1,y1)::(add l1 ((x2,y2)::l2))
            else
                (x2,y2)::(add ((x1,y1)::l1) l2);;

let rec soustract a b = match (a,b) with
    | (l,[]) -> l
    | ([],(x2,y2)::l) -> (-x2,y2)::(soustract [] l)
    | ((x1,y1)::l1,(x2,y2)::l2) ->
        if y1=y2 then
            if x1-x2 = 0 then
                (soustract l1 l2)
           else
                (x1-x2,y1)::(soustract l1 l2)
        else
            if y1>y2 then
                (-x2,y2)::(soustract a l2)
            else
                (x1,y1)::(soustract l1 b);;
