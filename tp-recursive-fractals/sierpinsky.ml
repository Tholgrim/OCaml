#load "graphics.cma";;
open Graphics ;;
open_graph "";;
  
let eponge (x,y) n =
  moveto x y;
  set_color black;
  fill_rect x y n n;
  set_color white; 
  let rec sierpinsky (x,y) n = function
    | 0 -> ()
    | p -> let n3 = (n/3) in
      let x1 = x+n3 and x2 = x + 2*n3 and y1 = y + n3 and y2 = y + 2*n3
      in
      fill_rect x1 y1 n3 n3;
      sierpinsky (x,y) n3 (p-1);
      sierpinsky (x1,y) n3 (p-1);
      sierpinsky (x2,y) n3 (p-1);
      sierpinsky (x,y1) n3 (p-1);
      sierpinsky (x2,y1) n3 (p-1);
      sierpinsky (x,y2) n3 (p-1);
      sierpinsky (x1,y2) n3 (p-1);
      sierpinsky (x2,y2) n3 (p-1)
  in sierpinsky (x,y) n 5;;

clear_graph();;
eponge (100,100) 500;;
