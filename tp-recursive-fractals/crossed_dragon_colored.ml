#load "graphics.cma";;
open Graphics;;
open_graph "";;

let rec dragon1 x y z t c = function
  | 0 -> (moveto x y; lineto z t)
  | n -> let m = (z+x)/2+(t-y)/2 and h = (y+t)/2-(z-x)/2 in
      set_color (rgb (c mod 256) (c*4 mod 256) (c*8 mod 256));
	  dragon1 x y m h (c+59) (n-1);
      dragon2 m h z t (c+40) (n-1)
and dragon2 x y z t c = function
  | 0 -> (moveto x y; lineto z t)
  | n -> let m = (z+x)/2-(t-y)/2 and h = (y+t)/2+(z-x)/2 in
      set_color (rgb (c*15 mod 256) (c mod 256) (c*5 mod 256));
	  dragon1 x y m h (c+72) (n-1);
      dragon2 m h z t (c+123) (n-1) ;;
	  
clear_graph();;
dragon1 200 500 700 500 0 18;;
