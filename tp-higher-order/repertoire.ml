type name = string
type age = int
type adress = string
type telephone = int
type contact = (name * age * telephone)
type agenda = contact list

(* Higher Order *)

(* 3. Tool Box *)

                (* 3.1 Lists *)

let rec exist x = function
        | [] -> false
        | e::l -> x = e || exist x l;;

let rec add_list x = function
        | [] -> [x]
        | e::l when x<e -> x::e::l
        | e::l when x>e -> e::(add_list x l)
        | _ -> failwith "Liste sans redondance";;

let rec remove_list x = function
        | e::l when e = x -> l
        | e::l -> e::(remove_list x l)
        | _ -> failwith "Empty list";;

                (* 3.2 Functions insertion on higher order *)

let rec map f = function
        | [] -> []
        | e::l -> f e :: map f l;;

let rec iter f = function
        | [] -> ()
        | e::l -> f e ; iter f l;;

let rec for_all p = function
        | [] -> true
        | e::l -> p e && for_all p l;;

                (* 3.3 Characters Handling *)

let upper_case c = match int_of_char c with
        | n when n > 64 && n < 91 -> char_of_int n
        | n when n > 96 && n < 123 -> char_of_int (n - 32)
        | _ -> failwith "Nope";;

let lower_case c = match int_of_char c with
        | n when n > 64 && n < 91 -> char_of_int (n + 32)
        | n when n > 96 && n < 123 -> char_of_int n
        | _ -> failwith "Nope";;
        
                (* 3.4 Strings Handling *)

let string_of_char c = String.make 1 c;;

let string_apply f s =
        let rec aux = function
                | 0 -> ""
                | n -> aux (n-1)^ string_of_char (f (char_of_int n))
        in
                aux (String.length s + 1);;

let string_uppercase = string_apply upper_case;;

let string_lowercase = string_apply lower_case;;

let convert_first_maj s =
        let rec aux = function
                | 0 -> ""
                | n -> aux (n-1)^(string_of_char s.[n])
        in
                string_of_char (upper_case s.[0]) ^ aux (String.length s - 1);;

        (* 4. Directory Creation *)

let create_contact (name,age,adress,phone):contact = 
        (convert_first_maj (string_lowercase name),age,adress,phone);;

let rec manage_contact f a b = f a b;;

let rec add_contact c = function
        
