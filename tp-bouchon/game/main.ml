#use "game.ml";;
#use "ai.ml";;

let ask_int() =
        let str = read_line () in
        let length = String.length str - 1 in
        int_of_string (
                if str.[length] = ';' then
                        String.sub str 0 (length - 1)
                else
                        str);;

let get_bet (pl:plate) (a:bool) (b:bool) =
        if a then
                play pl b
        else
                ask_int();;

let test_winner (x:game_status) = match x with
        | NotEnded -> false
        | Win1 -> print_string "Game Over : Player 1 wins \n"; true
        | Win2  -> print_string "Game Over : Player 2 wins \n"; true
        | Draw -> print_string "Game Over : Draw! \n"; true;;

let main (a:bool) (b:bool) i =
        let pli = init i in
        display pli;
        let rec aux pl =
                let n1 = get_bet pl a b and n2 = get_bet pl (not a) b in
                print_string "--------------------- \n";
                Printf.printf "P1 > %i \n" n1;
                Printf.printf "P2 > %i \n" n2;
                print_string "--------------------- \n";
                let plu = update pl (n1,n2) in
                display plu;
                if test_winner (get_status plu) then
                        ()
                else
                        aux plu
        in
                aux pli;;

main true true 100;;
