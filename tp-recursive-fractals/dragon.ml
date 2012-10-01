#load "graphics.cma";;
open Graphics;;
open_graph "";;

let rec dragon x y z t = function
  | 0 -> (moveto x y; lineto z t)
  | n -> let m = (z+x)/2+(t-y)/2 and h = (y+t)/2-(z-x)/2 in
      dragon x y m h (n-1);
      dragon z t m h (n-1) ;;

clear_graph();;
dragon 200 500 700 500 20;;
