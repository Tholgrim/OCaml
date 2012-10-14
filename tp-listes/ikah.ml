        (* 3 Travail en entreprise : Ik *)

	(* 3.1 Trouver les comptes du client *)

let compare a b = match (a,b) with
	| (a,b) when a = b -> 0
	| (a,b) when a < b -> -1
	| _ -> 1;;

let rec find c = function
	| [] -> failwith "This client doesn't exist."
	| (s,i)::l -> 
		if (String.lowercase s) = (String.lowercase c) then
			i
		else
			find c l;;

        (* 3.2 Ajouter un client *)

let rec add (c,v) = function
        | [] -> [(c,v)]
        | (s,i)::l -> match compare (String.lowercase c) (String.lowercase s) with
                | 0 -> (s,i)::l
                | -1 -> (c,v)::(s,i)::l
                | _ -> (s,i)::(add (c,v) l);;

        (* 3.3 Supprimer un client *)

let rec delete c = function
        | [] -> failwith "This client isn't in the database."
        | (s,i)::l -> 
                if s = c then
                        l
                else
                        (s,i)::(delete c l);;

        (* 3.4 Mise a jour du compte *)

let rec update c v = function
        | [] -> failwith "You are not a client."
        | (s,i)::l ->
                if s = c then
                        (s,v)::l
                else
                        (s,i)::(update c v l);;

        (* 3.5 Changement de nom *)

let name on nn = function
        | _ when on = nn -> failwith "You are trying to change your old name by the same new name."
        | lc -> 
                delete on (add (nn,(find on lc)) lc);;

        (* 3.6 Joyeux Joel! *)

let rec map f = function
        | [] -> []
        | e::l -> f e::(map f l);;

let rec sales l r = 
        match l with
        | [] -> []
        | l -> map (function (x,y) -> (x , ((100. -. r )/.100.)*.y)) l;;

        (* 3.7 Fusion Ik&Ah *)

let rec fusion = function
        | (l,[]) | ([],l) -> l
        | ((s1,i1)::l1,(s2,i2)::l2) -> 
                match compare s1 s2 with
                | 0 -> (s1,i1+.i2)::(fusion (l1,l2))
                | 1 -> (s2,i2)::(fusion ((s1,i1)::l1,l2))
                | _ -> (s1,i1)::(fusion (l1, (s2,i2)::l2));;

        (* 3.8 Bonus : tri fusion *)

let divide l =
        let rec divrec = function
                | (x,[]) | (x,_::[]) -> ([],x)
                | (e::l1,_::_::l2) -> 
                        let (n1,n2) = divrec (l1,l2) in
                                (e::n1,n2)
                | _ -> ([],[])
        in
                divrec (l,l);;

let rec merge = function
        | (l,[]) | ([],l) -> l
        | ((sa,ia)::la,(sb,ib)::lb) -> match compare (String.lowercase sa) (String.lowercase sb) with
                | -1 -> (sa,ia)::(merge (la,((sb,ib)::lb)))
                | _ -> (sb,ib)::(merge (((sa,ia)::la),lb));;

let rec merge_sort = function
        | ([_] | []) as l -> l
        | l -> let (l1,l2) = divide l in
                merge ((merge_sort l1),(merge_sort l2));;

        (* 3.9 Bonus : chiffrement de Caesar *)


