type plate = int*int*int;;

type game_status =
          NotEnded
        | Win1
        | Win2
        | Draw;;

let init n = ((0,n,n):plate);;

let display ((p,g1,g2):plate) =
        Printf.printf ("Position : %i \n") p;
        Printf.printf ("Pl. 1 : %i \n") g1;
        let rec aux = function
                | 3 -> ()
                | n when n = p -> print_string "  |  X  |\n"; aux (n+1)
                | n -> print_string "  |     |\n"; aux (n+1)
        in
                aux (-2);
        Printf.printf ("Pl. 2 : %i \n") g2;;

let update ((p,g1,g2):plate) = function
        | (n1,n2) when n1 < n2 -> ((p+1,g1-n1,g2-n2):plate)
        | (n1,n2) when n1 > n2 -> ((p-1,g1-n1,g2-n2):plate)
        | (n1,n2) -> ((p,g1-n1,g2-n2):plate);;

let get_status ((p,g1,g2):plate) = match (p,g1,g2) with
        | (_,g1,g2) when (g1 = g2 && g1 = 0) -> Draw
        | (p,_,_) when p = 2 -> Win2
        | (p,_,_) when p = -2 -> Win1
        | _ -> NotEnded;;
