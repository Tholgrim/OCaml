let create () =
        [];;

let singleton a b =
        [(a,[b])];;

let rec length = function
        | [] -> 0
        | e::l -> 1 + length l;;

let rec size = function
        | [] -> 0
        | (a,ls)::l -> 1 + length ls + size l;;

let add k v l =
        let rec aux = function
                | [] -> [(k,[v])]
                | (ek,el)::l when ek = k -> (ek,v::el)::l
                | k::l -> k::(aux l)
        in
                aux l;;

let rec mem k = function
        | [] -> false
        | (ek,_)::l when k = ek -> true
        | (ek,_)::l -> mem k l;;

let rec get k = function
        | (ek,ev::el)::l when k = ek -> ev
        | (_,_)::l -> get k l
        | _ -> failwith "Not_found";;

let rec get_all k = function
        | (ek,el)::l when k = ek -> el
        | (_,_)::l -> get_all k l
        | _ -> failwith "Not_found";;

let rec replace k lv = function
        | (ek,el)::l when k = ek -> (ek,lv)::l
        | (ek,el)::l -> (ek,el)::(replace k lv l)
        | _ -> failwith "Not_found";;

let rec remove k = function
        | (ek,el)::l when k = ek -> l
        | (ek,el)::l -> (ek,el)::(remove k l)
        | _ -> failwith "Not_found";;

let rec iter f = function
        | [] | (_,[])::_ -> ()
        | (ek,ev::el)::l -> f ek ev el; print_newline(); iter f l;;

let rec map f = function
        | [] -> []
        | (ek,[])::l -> (ek,[])::(map f l);
        | (ek,ev::el)::l -> (ek,(f ek ev)::el)::(map f l);;

let rec filter p = function
        | [] -> []
        | (ek,[])::l -> filter p l
        | (ek,ev::el)::l when p ek ev -> (ek,ev)::(filter p l)
        | (ek,ev::el)::l -> filter p l;;
