let play pl isP1 =
        if isP1 then
                match pl with
                | (_,a,_) -> 
                                if a > 3 then
                                        3
                                else
                                        if a >= 1 then
                                                1
                                        else
                                                0
        else
                match pl with
                | (_,_,a) -> 
                                if a > 3 then
                                        3
                                else
                                        if a >= 1 then
                                                1
                                        else
                                                0;;

