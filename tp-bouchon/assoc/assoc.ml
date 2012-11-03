let create () =
        [];;

let singleton a b =
        [(a,[b])];;

let rec length = function
        | [] -> 0
        | _::l -> 1 + length l;;

let rec size = function
        | [] -> 0
        | (a,ls)::l -> length ls + size l;;

let rec add k v = function
        | [] -> [(k,[v])]
        | (ek,lv)::l when ek = k -> (ek,v::lv)::l
        | e::l -> e::(add k v l);;

let rec mem k = function
        | [] -> false
        | (ek,_)::l when k = ek -> true
        | _::l -> mem k l;;

let rec get k = function
        | [] -> failwith "Not_found"
        | (ek,ev::_)::l when k = ek -> ev
        | _::l -> get k l;;

let rec get_all k = function
        | (ek,lv)::l when k = ek -> lv
        | _::l -> get_all k l
        | _ -> failwith "Not_found";;

let rec replace k v = function
        | (ek,_)::l when k = ek -> (ek,v)::l
        | (ek,lv)::l -> (ek,lv)::(replace k v l)
        | _ -> failwith "Not_found";;

let rec remove k = function
        | (ek,lv)::l when k = ek -> l
        | (ek,lv)::l -> (ek,lv)::(remove k l)
        | _ -> failwith "Not_found";;

let rec iter f = function
        | [] | (_,[])::_ -> ()
        | (ek,ev::lv)::l -> f ek ev; print_newline(); iter f l;;

let rec map f = function
        | [] -> []
        | (ek,[])::l -> (ek,[])::(map f l);
        | (ek,ev::lv)::l -> (ek,(f ek ev)::lv)::(map f l);;

let rec filter p = function
        | [] -> []
        | (ek,[])::l -> filter p l
        | (ek,ev::lv)::l when p ek ev -> (ek,ev)::(filter p l)
        | _::l -> filter p l;;

(* Bonus *)

let rec memq k v = function
        | [] -> false
        | (ek,lv)::l when ek = k ->
                        let rec aux = function
                                | [] -> false
                                | ev::lv when ev = v -> true
                                | _::lv -> aux lv
                        in
                                aux lv
        | _::l -> memq k v l;;

let rec removeq k v = function
        | [] -> []
        | (ek,lv)::l when ek = k ->
                        let rec aux = function
                                | [] -> []
                                | ev::lv when ev = v -> lv
                                | ev::lv -> ev::(aux lv)
                        in
                                (ek,(aux lv))::l
        | (ek,lv)::l -> (ek,lv)::(removeq k v l);;

let rec iter_all f = function
        | [] | (_,[])::_ -> ()
        | (ek,lv)::l ->
                        let rec aux = function
                                | [] -> ()
                                | ev::lv -> f ek ev; print_newline(); aux lv
                        in
                                aux lv; iter_all f l;;

let rec map_all f = function
        | [] -> []
        | (ek,lv)::l ->
                        let rec aux = function
                                | [] -> []
                                | e::l -> f ek e :: (aux l)
                        in
                                (ek,(aux lv))::(map_all f l);;

let rec for_all p = function
        | [] -> true
        | (ek,lv)::l ->
                        let rec aux = function
                                | [] -> true
                                | ev::lv -> p ek ev && aux lv
                        in
                                aux lv && for_all p l;;

let rec exists p = function
        | [] -> false
        | (ek,lv)::l ->
                        let rec aux = function
                                | [] -> false
                                | ev::lv -> p ek ev || aux lv
                        in
                                aux lv || for_all p l;;

let rec filter_all p = function
        | [] -> []
        | (ek,lv)::l ->
                        let rec aux = function
                                | [] -> true
                                | ev::lv -> (p ek ev) && (aux lv)
                        in
                                if aux lv then
                                        (ek,lv)::(filter_all p l)
                                else
                                        filter_all p l;;
